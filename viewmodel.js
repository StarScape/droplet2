import { Paragraph, Selection, Run } from './Paragraph.js'
import CharRuler from './CharRuler.js'

const addMax = (viewmodel, run, leftovers, ruler) => {
  const rest = viewmodel.addMaxChars(run, ruler)
  if (!rest.empty) {
    leftovers.push(rest)
  }
}

class ParagraphViewModel {
  static fromParagraph(p, containerWidth, ruler) {
    const vm = new ParagraphViewModel(p, containerWidth)
    const leftovers = [] // leftovers from last line

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

class ViewModelLine {
  get lastSpan() {
    return this.spans[this.spans.length-1]
  }

  get length() {
    return this.spans.reduce((length, s) => length + s.length, 0)
  }

  constructor(containerWidth, offset) {
    this.spans = []
    this.width = 0
    this.containerWidth = containerWidth
    this.offset = offset
  }

  addSpan(formats) {
    const offset = this.spans.length > 0 ?  this.lastSpan.offset + this.lastSpan.length : this.offset
    const newSpan = new ViewModelSpan(offset, formats)
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
}

class ViewModelSpan {
  get length() {
    return this.text.length
  }

  constructor(offset, formats = []) {
    this.text = ''
    this.offset = offset
    this.formats = formats
    this.width = 0
  }
}

// Split string into lines, each line as long as possible but not exceeding containerWidth
// TODO: refactor to use classes `Line` and `Span` and `ParagraphViewModel`? Might be more clear/explicit.
const lineify = (paragraph, containerWidth, ruler) => {
  let widthOfPrevSpans = 0
  const lines = [{
    paragraph: paragraph,
    spans: [],
  }]

  for (let run of paragraph.runs) {
    const words = run.text.split(/(\s+)/g)
    lines[lines.length-1].spans.push({
      text: '',
      formats: run.formats,
      width: 0,
      offset: null,
    })

    for (let word of words) {
      const spans = lines[lines.length-1].spans
      const currentSpan = spans[spans.length-1]

      // We DON'T want to measure white-space that falls at the end of a line, hence the trim().
      const spanTextNew = currentSpan.text + word
      const spanWidthNew = ruler.measureString(spanTextNew.trim(), currentSpan.formats)
      const lineWidthNew = widthOfPrevSpans + spanWidthNew

      if (Math.floor(lineWidthNew) <= containerWidth) {
        currentSpan.text += word
        currentSpan.width = ruler.measureString(spanTextNew, currentSpan.formats)
      }
      else {
        // No more space on line, make new one
        widthOfPrevSpans = 0

        lines.push({
          paragraph: paragraph,
          spans: [{
            text: word,
            formats: run.formats,
            width: 0,
            offset: null,
          }],
        })
      }
    }

    const currentLine = lines[lines.length-1]
    widthOfPrevSpans += currentLine.spans[currentLine.spans.length-1].width
  }

  // TODO: disgusting, fix this
  let cumCharOffset = 0
  for (let line of lines) {
    for (let span of line.spans) {
      span.length = span.text.length
      span.offset = cumCharOffset
      cumCharOffset += span.length
    }
  }

  // TODO: replace with ViewModel#length
  lines.paragraphLength = paragraph.length

  return lines
}

const moveCaretDown = (viewmodel, offset) => {
  // TODO
}

const _makeCursorElem = () => {
  const elem = document.createElement('span')
  elem.classList.add('text-caret')
  return elem
}

const _makeSpanElem = (text, formats) => {
  const spanElem = document.createElement('span')
  spanElem.innerHTML = text

  // Apply style
  for (let format of formats) {
    if (format === 'bold') {
      spanElem.style.fontWeight = 'bold'
    }
    else if (format === 'italic') {
      spanElem.style.fontStyle = 'italic'
    }
    // TODO: rest of formats
  }

  return spanElem
}

const renderViewModel = (viewmodel, caret) => {
  let html = ''

  for (let line of viewmodel.lines) {
    const lineElem = document.createElement('div')

    for (let span of line.spans) {
      const start = span.offset
      const end = start + span.length

      if ((caret >= start && caret < end) || (end === viewmodel.length && caret === end)) {
        const spanOffset = caret - span.offset
        const before = span.text.slice(0, spanOffset)
        const after = span.text.slice(spanOffset, span.text.length) // TODO: replace with span.splitAt(spanOffset)

        const span1 = _makeSpanElem(before, span.formats)
        const span2 = _makeSpanElem(after, span.formats)
        const cursor = _makeCursorElem()

        lineElem.appendChild(span1)
        lineElem.appendChild(cursor)
        lineElem.appendChild(span2)
      }
      else {
        const spanElem = _makeSpanElem(span.text, span.formats)
        lineElem.appendChild(spanElem)
      }
    }

    html += lineElem.outerHTML
  }

  return html
}

const fakeEditor = document.getElementById('fake-editor')
const style = getComputedStyle(fakeEditor)
const fontSize = style.getPropertyValue('font-size')
const fontFamily = style.getPropertyValue('font-family')

// TODO: write test with this exact example
const ruler = new CharRuler(fontSize, fontFamily)
const para = new Paragraph([
  new Run("Hello world, this is an example of a paragraph ", []),
  new Run("that I might want to split into lines. I'm really just typing a bunch of random stuff in here. ", ['italic']),
  new Run("Don't know what else to say. Hmmmm...", ['bold'])
])

let caret = 0


document.addEventListener('keydown', (e) => {
  // TODO: this logic should be shifted to using Selection#shiftSingle
  if (e.code === 'ArrowLeft' && caret > 0) {
    caret--
  }
  else if (e.code === 'ArrowRight' && caret < para.length) {
    caret++
  }

  const vm = ParagraphViewModel.fromParagraph(para, 200, ruler)
  fakeEditor.innerHTML = renderViewModel(vm, caret)
})
