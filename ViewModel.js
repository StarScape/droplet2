import { Run } from './Paragraph.js'

const addMax = (viewmodel, run, leftovers, ruler) => {
  const rest = viewmodel.addMaxChars(run, ruler)
  if (!rest.empty) {
    leftovers.push(rest)
  }
}

/**
 * The ViewModel is an intermediate representation between the core editor data types -> HTML.
 * An intermediate stage is necessary because handle the splitting of paragraphs into lines ourselves,
 * rather than having the browser do it for us -- this in turn is needed because we draw our own text caret
 * and selections, and in order to have fine-grained control of layout-sensitive operations such as navigating
 * the caret to the end of the line (⌘ + → on macOS, End on Windows/Linux).
 *
 * The DOM APIs for handling selection are not terribly user-friendly either, or particularly conducive to the
 * idea of decoupling your underlying data respresentation from how it is rendered. For example, the Selection
 * API relies on an anchor node and an offset into it, but if anchorNode is an element and not a text node, the
 * offset will be the *number of elements* into the anchor node, not the *text* offset, which is obviously what
 * we're interested in here, since we're dealing with manipulations on text.
 *
 * For this and other reasons, manually calculating lines into a 'ViewModel' -- and then keeping track of how the text
 * offsets of these correspond to those inside of the model -- winds up being a better solution.
 *
 * Oh, and don't get too hung up on the term 'ViewModel' -- I choose that simply because it's an intermediate
 * representation between the model and the view, not because it's directly inspired by any of the MVC/psuedo-MVC
 * frameworks that also happen to use that term.
 *
 */
export class ParagraphViewModel {
  static fromParagraph(p, containerWidth, ruler) {
    const vm = new ParagraphViewModel(p, containerWidth)
    const leftovers = [] // leftovers from last line, used as queue

    for (const run of p.runs) {
      addMax(vm, run, leftovers, ruler)

      while (leftovers.length > 0) {
        const leftoverRun = leftovers.shift()
        addMax(vm, leftoverRun, leftovers, ruler)
      }
    }

    return vm
  }

  get lastLine() {
    return this.lines[this.lines.length-1]
  }

  get length() {
    return this.paragraph.length
  }

  constructor(paragraph, containerWidth) {
    this.paragraph = paragraph
    this.containerWidth = containerWidth
    this.lines = []
    this.addLine()
  }

  addLine() {
    const offset = this.lines.length > 0 ? this.lastLine.offset + this.lastLine.length : 0
    const newLine = new ViewModelLine(this.containerWidth, offset)
    this.lines.push(newLine)
  }

  // Adds as many words from `run` as possible to last line without overflowing it
  addMaxChars(run, ruler) {
    const rest = this.lastLine.addMaxChars(run, ruler)
    if (!rest.empty) {
      this.addLine()
    }
    return rest
  }
}

export class ViewModelLine {
  get lastSpan() {
    return this.spans[this.spans.length-1]
  }

  get length() {
    return this.spans.reduce((length, s) => length + s.length, 0)
  }

  /**
   * @return Offset of VISUAL last character in a line. Note there is an 'invisible'
   *         space at the end of any line whose text overflows into the next.
   */
  get endOffset() {
    const lastSpan = this.spans[this.spans.length - 1]

    // There's an invisible space at the end of
    // every line except the last in a paragraph.
    if (lastSpan.text[lastSpan.text.length - 1] === ' ') {
      return this.offset + this.length - 1
    }

    return this.offset + this.length
  }

  constructor(containerWidth, offset) {
    this.spans = []
    this.width = 0
    this.containerWidth = containerWidth

    /**
     * Beginning offset of line within <em>paragraph</em>, not the line itself.
     */
    this.offset = offset
  }

  addSpan(formats) {
    const offset = this.spans.length > 0 ?  this.lastSpan.offset + this.lastSpan.length : this.offset
    const newSpan = new ViewModelSpan('', offset, formats)
    this.spans.push(newSpan)
    return newSpan
  }

  // Adds as many words from `run` as possible without overflowing line
  addMaxChars(run, ruler) {
    const span = this.addSpan(run.formats)
    const words = run.text.split(/(\s+)/g)
    let offset = 0

    let rest = Run.empty()

    for (let word of words) {
      // We DON'T want to measure whitespace that falls at the end of a line, hence the trim().
      const spanTextNew = span.text + word
      const spanWidthNew = ruler.measureString(spanTextNew.trim(), span.formats)
      const lineWidthNew = this.width + spanWidthNew

      if (Math.floor(lineWidthNew) <= this.containerWidth) {
        offset += word.length
        const realSpanWidth = ruler.measureString(spanTextNew, span.formats)

        span.text += word
        span.width = realSpanWidth
      }
      else {
        rest = run.split(offset)[1]
        break
      }
    }

    this.width += span.width
    return rest
  }

  /**
   * Returns everything in the line before and after the provided paragraph offset.
   *
   * @param {number} offset - Paragraph offset to split on.
   * @return {array} Two arrays: one with everything before offset (inclusive), one with everything after.
   */
  split(offset) {
    const before = []
    const after = []

    for (let span of this.spans) {
      const spanEndOffset = span.offset + span.length

      // Offset to split at lies after current span
      if (spanEndOffset <= offset) {
        before.push(span)
      }
      // Offset to split at lies in current span
      else if (span.offset <= offset && offset < spanEndOffset) {
        const [left, right] = span.split(offset)
        before.push(left)
        after.push(right)
      }
      // Offset to split at lies before current span
      else if (span.offset >= offset) {
        after.push(span)
      }
      else {
        throw new Error('Offset is not in line. This should never happen.')
      }
    }

    return [before, after]
  }

  /**
   * Measures the width of the line with the provided ruler.
   *
   * @return {number} Width in px.
   *
   */
  measure(ruler) {
    return this.spans.reduce((sum, s) => sum + s.measure(ruler), 0)
  }
}

export class ViewModelSpan {
  static empty() {
    return new ViewModelSpan('', 0)
  }

  get length() {
    return this.text.length
  }

  get empty() {
    return (
      this.text === null ||
      this.text === undefined ||
      this.text === ''
    )
  }

  constructor(text, offset, formats = []) {
    this.text = text
    /**
     * Beginning offset of span within  the <em>paragraph</em>, not the line itself.
     */
    this.offset = offset
    this.formats = formats
    this.width = 0
  }

  /**
   * Splits the span at `offset` and returns a 2-element array
   * containing the contents of the span on either side of the offset.
   *
   * @param {number} offset - Paragraph offset to split run at.
   *
   * @return {array} [beforeOffset, afterOffset]
   *
   */
  split(offset) {
    const spanOffset = offset - this.offset

    const beforeText = this.text.slice(0, spanOffset)
    const afterText = this.text.slice(spanOffset, this.length)
    const before = new ViewModelSpan(beforeText, this.offset, this.formats)
    const after = new ViewModelSpan(afterText, offset, this.formats)

    return [before, after]
  }

  /**
   * Measures the width of the span with the provided ruler.
   *
   * @return {number} Width in px.
   *
   */
  // TODO: do we need this?
  measure(ruler) {
    return ruler.measureString(this.text, this.formats)
  }
}

