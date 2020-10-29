import { Paragraph, Selection, Run, Selection as MySelection } from './Paragraph.js'
import { ParagraphViewModel, ViewModelSpan } from './ViewModel.js'
import CharRuler from './CharRuler.js'

/***   Helper functions   ***/

// TODO: Test ;)
const formatsToStyleStr = (formats) => {
  let str = ''

  for (let format of formats) {
    if (format === 'bold') {
      str += 'font-weight: bold;'
    }
    else if (format === 'italic') {
      str += 'font-style: italic;'
    }
    else {
      // TODO: rest of formats
      throw new Error('unimplemented format')
    }
  }

  return str
}

/**
 * Does domElem have any padding set?
 */
const hasPadding = (domElem) => {
  const style = getComputedStyle(domElem)
  const paddingSides = ['padding-top', 'padding-right', 'padding-bottom', 'padding-left']

  for (let side of paddingSides) {
    if (parseFloat(style.getPropertyValue(side)) !== 0) {
      return true
    }
  }

  return false
}

/**
 * @return Actual measured line height of paragraph DOM element.
 */
const getLineHeight = (paragraphDomElem) => {
  const lines = paragraphDomElem.querySelectorAll('.line')
  const lineHeights = Array.from(lines).map(line =>
    parseFloat(getComputedStyle(line).getPropertyValue('height'))
  )

  // TODO: is this needed? Can we just yeet the first one?
  return Math.max(...lineHeights)
}

// Returns line the caret is currently in
const getCaretLine = (viewmodel, selection) => {
  if (!selection.single) {
    throw new Error('Cannot get caret line for a multiple selection!')
  }

  let i = 0
  for (let line of viewmodel.lines) {
    const start = line.offset
    const end = start + line.length

    const withinLine = (selection.caret >= start && selection.caret < end)
    const atParagraphEnd = (selection.caret === end && end === viewmodel.length)

    if (withinLine || atParagraphEnd) {
      return i
    }

    i++
  }

  // TODO: Is this a good idea...?
  throw new Error("Line is not inside of this paragraph's viewmodel.")
}

// Returns cumulative width of a list of spans, measured with `ruler`.
const measureSpans = (spans, ruler) => {
  return spans.reduce((width, sp) => width + sp.measure(ruler), 0)
}

// Returns how many pixels the caret is from the left.
const getCaretPx = (selection, line, ruler) => {
  const beforeCaret = line.split(selection.caret)[0]
  const caretPx = measureSpans(beforeCaret, ruler)
  return caretPx
}

// Returns the offset inside `line` which is *closest* to `px` pixels from the left of line start.
// Useful for up/down caret operations.
// As with all viewmodel operations, the returned offset is relative to the *paragraph*, not the line.
const getNearestLineOffsetToPixel = (line, px, ruler) => {
  const lineEnd = line.offset + line.length

  let currentOffset = line.offset
  let currentOffsetPx = 0
  let prevDelta = Infinity

  for (let span of line.spans) {
    for (let char of span.text) {
      const delta = Math.abs(currentOffsetPx - px)
      if (delta > prevDelta) {
        return currentOffset - 1
      }

      prevDelta = delta
      currentOffsetPx += ruler.measure(char, span.formats)
      currentOffset++
    }
  }

  // The *last character* in every line is actually a space, and the editor will display.
  // the text caret at the beginning of the *next* line down.
  // We need the character just before it.
  return line.endOffset
}

/***   Main operations   ***/

/**
 * Returns paragraph offset clicked.
 *
 * @param {Event} e - Click event
 * @param {HTMLDomElement} domElem - Paragraph element clicked
 * @param {ParagraphViewModel} viewmodel - ViewModel of clicked paragraph
 * @param {CharRuler} ruler - Ruler to measure with
 */
const clickedAt = (e, domElem, viewmodel, ruler) => {
  if (hasPadding(domElem)) {
    throw new Error(
      "Paragraph elements CANNOT have padding. This will break certain rendering calculations!"
    );
  }

  const lineHeight = getLineHeight(domElem)
  const domElemRect = domElem.getBoundingClientRect()
  const px = e.clientX - domElemRect.x
  const py = e.clientY - domElemRect.y

  // Get appropriate line
  let line = null
  if (py < 0) {
    line = viewmodel.lines[0]
  } else if (py > domElemRect.height) {
    line = viewmodel.lines[viewmodel.lines.length-1]
  } else {
    line = viewmodel.lines[Math.floor(py / lineHeight)]
  }

  // Get appropriate position within line
  let offset = null
  if (px < 0) {
    offset = line.offset
  } else if (px > domElemRect.width) {
    offset = line.endOffset
  } else {
    offset = getNearestLineOffsetToPixel(line, px, ruler)
  }

  return offset
}

const downArrow = (viewmodel, selection, ruler) => {
  const collapsed = selection.collapse()
  const lineIdx = getCaretLine(viewmodel, collapsed)
  const line = viewmodel.lines[lineIdx]

  if (lineIdx < viewmodel.lines.length-1) {
    const nextLineIdx = lineIdx + 1
    const nextLine = viewmodel.lines[nextLineIdx]
    const caretOffsetPx = getCaretPx(collapsed, line, ruler)
    const downOffset = getNearestLineOffsetToPixel(nextLine, caretOffsetPx, ruler)

    return collapsed.setSingle(downOffset)
  }

  return collapsed
}

const upArrow = (viewmodel, selection, ruler) => {
  const collapsed = selection.collapse()
  const lineIdx = getCaretLine(viewmodel, collapsed)
  const line = viewmodel.lines[lineIdx]

  if (lineIdx > 0) {
    const prevLine = viewmodel.lines[lineIdx - 1]
    const caretOffsetPx = getCaretPx(collapsed, line, ruler)
    const downOffset = getNearestLineOffsetToPixel(prevLine, caretOffsetPx, ruler)

    return collapsed.setSingle(downOffset)
  }

  return collapsed
}

// TODO: Add methods for shiftUpArrow() and shiftDownArrow() that expand selection appropriately.
// These can possibly wait until the real implementation.

const leftArrow = (selection) => {
  if (selection.range) {
    return selection.collapseStart()
  }
  else if (selection.single && selection.caret > 0) {
    return selection.moveSingle(-1)
  }

  return selection
}

const shiftLeftArrow = (selection) => {
  if (selection.start.offset > 0) {
    if (selection.range && !selection.backwards) {
      return selection.expandRight(-1)
    }
    return selection.expandLeft(1)
  }
  return selection
}

const rightArrow = (selection) => {
  if (selection.range) {
    return selection.collapseEnd()
  }
  else if (selection.single && selection.end.offset < selection.end.paragraph.length) {
    return selection.moveSingle(1)
  }
  return selection
}

const shiftRightArrow = (selection) => {
  if (selection.end.offset < selection.end.paragraph.length) {
    if (selection.range && selection.backwards) {
      return selection.expandLeft(-1)
    }
    return selection.expandRight(1)
  }
  return selection
}

/***   Rendering   ***/
const PARAGRAPH_CLASS = 'paragraph'
const LINE_CLASS = 'line'
const SELECTION_CLASS = 'range-selection'

const renderCaret = () => {
  return `<span class='text-caret'></span>`
}

const renderSpan = (span) => {
  if (span.empty) {
    return ''
  }

  const styleStr = formatsToStyleStr(span.formats)
  return `<span style='${styleStr}'>${span.text}</span>`
}

// TODO: Sit down with a sheet of paper and rethink how this function ought
// to work. In its current state it's over-complicated and not very maintainable.
const renderViewModel = (viewmodel, selection) => {
  let html = `<div class='${PARAGRAPH_CLASS}'>`
  let selectionOngoing = false

  for (let line of viewmodel.lines) {
    let lineElem = `<div class='${LINE_CLASS}'>`

    for (let span of line.spans) {
      const start = span.offset
      const end = start + span.length
      const withinSpan = (offset) =>
        (offset >= start && offset < end) || (offset === viewmodel.length && offset === end)

      // Draw caret
      if (selection.single && withinSpan(selection.caret)) {
        const [span1, span2] = span.split(selection.caret).map(s => renderSpan(s))
        lineElem += (span1 + renderCaret() + span2)

        span = ViewModelSpan.empty()
      }

      // Draw range selection
      if (selection.range) {
        if (selectionOngoing) {
          lineElem += `<span class='${SELECTION_CLASS}'>`
        }

        if (withinSpan(selection.start.offset)) {
          const [before, after] = span.split(selection.start.offset)

          lineElem += renderSpan(before) + `<span class='${SELECTION_CLASS}'>`
          if (selection.backwards) {
            lineElem += renderCaret()
          }
          span = after
          selectionOngoing = true
        }

        if (withinSpan(selection.end.offset)) {
          selectionOngoing = false

          const [before, after] = span.split(selection.end.offset)
          lineElem += renderSpan(before) + '</span>'
          if (!selection.backwards) {
            lineElem += renderCaret()
          }
          span = after
        }
      }

      lineElem += renderSpan(span)
      if (selectionOngoing) {
        lineElem += '</span>'
      }
    }

    html += lineElem + '</div>'
  }

  return html + '</div>'
}

// Editor
const fakeEditor = document.getElementById('fake-editor')
const hiddenInput = document.querySelector('#hidden-input')
const override = document.getElementById('override-checkbox')
const style = getComputedStyle(fakeEditor)
const fontSize = style.getPropertyValue('font-size')
const fontFamily = style.getPropertyValue('font-family')

// TODO: write test with this exact example
const ruler = new CharRuler(fontSize, fontFamily)
const _para = new Paragraph([
  new Run("Hello world, this is an example of a paragraph ", []),
  new Run("that I might want to split into lines. I'm really just typing a bunch of random stuff in here. ", ['italic']),
  new Run("Don't know what else to say. Hmmmm...", ['bold'])
])

// State
let paragraph = _para
let selection = new MySelection({ paragraph: paragraph, offset: 0 })
let viewmodel = ParagraphViewModel.fromParagraph(paragraph, 200, ruler)

const syncDom = () => {
  viewmodel = ParagraphViewModel.fromParagraph(paragraph, 200, ruler)
  fakeEditor.innerHTML = renderViewModel(viewmodel, selection)
}
syncDom()

fakeEditor.addEventListener('click', (e) => {
  // Eventually, I will need to think about handling events for any number of paragraphs. For now, this will do :3
  // document.querySelector('#hidden-input').focus()
  const paragraphElem = document.querySelector('.paragraph')
  const offset = clickedAt(e, paragraphElem, viewmodel, ruler)
  hiddenInput.focus()

  selection = selection.collapse().setSingle(offset)
  syncDom()
})

document.addEventListener('keydown', (e) => {
  if (override.checked) {
    return
  }

  if (e.code === 'ArrowLeft') {
    e.preventDefault()
    selection = e.shiftKey ? shiftLeftArrow(selection) : leftArrow(selection)
  }
  else if (e.code === 'ArrowRight') {
    e.preventDefault()
    selection = e.shiftKey ? shiftRightArrow(selection) : rightArrow(selection)
  }
  else if (e.code === 'ArrowDown') {
    e.preventDefault()
    selection = downArrow(viewmodel, selection, ruler)
  }
  else if (e.code === 'ArrowUp') {
    e.preventDefault()
    selection = upArrow(viewmodel, selection, ruler)
  }

  syncDom()
})

hiddenInput.addEventListener('beforeinput', (e) => {
  if (override.checked) {
    return
  }

  if (e.inputType === 'insertText') {
    const run = new Run(e.data, []);
    const [newParagraph, newSelection] = paragraph.insert([run], selection)

    selection = newSelection
    paragraph = newParagraph

    // Terrible hack, avert your eyes, it's just for the mockup...
    selection.start.paragraph = newParagraph
    selection.end.paragraph = newParagraph

    syncDom()
  }
  else if (e.inputType === 'deleteContentBackward') {
    if (selection.single && selection.caret === 0) {
      return
    }

    const [newParagraph, newSelection] = paragraph.remove(selection)
    selection = newSelection
    paragraph = newParagraph

    // That awful hack again :3
    selection.start.paragraph = newParagraph
    selection.end.paragraph = newParagraph

    syncDom()
  }
})

hiddenInput.addEventListener('focus', (e) => {
  document.querySelector('.text-caret').classList.remove('hidden')
  document.querySelectorAll('.range-selection').forEach(e =>
    e.classList.remove('range-selection-blurred')
  )
})

hiddenInput.addEventListener('blur', (e) => {
  document.querySelector('.text-caret').classList.add('hidden')
})

document.addEventListener('click', (e) => {
  // Unfocus range-selection
  if (!e.path.includes(fakeEditor)) {
    document.querySelectorAll('.range-selection').forEach(e =>
      e.classList.add('range-selection-blurred')
    )
  }
})
