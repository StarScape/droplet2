import { Paragraph, Selection, Run, Selection as MySelection } from './Paragraph.js'
import { ParagraphViewModel } from './ViewModel.js'
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

// Returns line the caret is currently in
const getCaretLine = (viewmodel, selection) => {
  if (!selection.single) {
    throw new Error('Not yet implemented.')
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
const caretDown = (viewmodel, selection, ruler) => {
  const lineIdx = getCaretLine(viewmodel, selection)
  const line = viewmodel.lines[lineIdx]

  if (lineIdx < viewmodel.lines.length-1) {
    const nextLineIdx = lineIdx + 1
    const nextLine = viewmodel.lines[nextLineIdx]
    const caretOffsetPx = getCaretPx(selection, line, ruler)
    const downOffset = getNearestLineOffsetToPixel(nextLine, caretOffsetPx, ruler)

    return selection.setSingle(downOffset)
  }

  return selection
}

const caretUp = (viewmodel, selection, ruler) => {
  const lineIdx = getCaretLine(viewmodel, selection)
  const line = viewmodel.lines[lineIdx]

  if (lineIdx > 0) {
    const prevLine = viewmodel.lines[lineIdx - 1]
    const caretOffsetPx = getCaretPx(selection, line, ruler)
    const downOffset = getNearestLineOffsetToPixel(prevLine, caretOffsetPx, ruler)

    return selection.setSingle(downOffset)
  }

  return selection
}

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

const renderViewModel = (viewmodel, selection) => {
  let html = ''
  if (!selection.single) {
    throw new Error('Have not handled case for range-selection yet')
  }

  for (let line of viewmodel.lines) {
    const lineElem = document.createElement('div')

    for (let span of line.spans) {
      const start = span.offset
      const end = start + span.length
      const caret = selection.caret

      if ((caret >= start && caret < end) || (end === viewmodel.length && caret === end)) {
        const spanOffset = caret - span.offset
        const [span1, span2] = span.split(caret).map(s => renderSpan(s))
        const caretElem = renderCaret()

        lineElem.innerHTML += (span1 + caretElem + span2)
      }
      else {
        lineElem.innerHTML += renderSpan(span)
      }
    }

    html += lineElem.outerHTML
  }

  return html
}

const fakeEditor = document.getElementById('fake-editor')
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
let selection = new MySelection({ paragraph: paragraph, offset: 134 })
let viewmodel = ParagraphViewModel.fromParagraph(paragraph, 200, ruler)

const syncDom = () => {
  fakeEditor.innerHTML = renderViewModel(viewmodel, selection)
}

syncDom()

document.addEventListener('keydown', (e) => {
  if (override.checked) {
    return
  }

  if (e.code === 'ArrowLeft' && selection.caret > 0) {
    e.preventDefault()
    selection = selection.shiftSingle(-1)
  }
  else if (e.code === 'ArrowRight' && selection.caret < paragraph.length) {
    e.preventDefault()
    selection = selection.shiftSingle(1)
  }
  else if (e.code === 'ArrowDown') {
    e.preventDefault()
    selection = caretDown(viewmodel, selection, ruler)
  }
  else if (e.code === 'ArrowUp') {
    e.preventDefault()
    selection = caretUp(viewmodel, selection, ruler)
  }

  syncDom()
})

document.addEventListener('beforeinput', (e) => {
  // TODO: Finish this :)
  if (e.inputType === 'insertText') {
    const [newSelection, newParagraph] = paragraph.insert(e.keyOrWhatever, selection)
    const newVm = ParagraphViewModel.fromParagraph(newParagraph)

    fakeEditor.innerHTML = renderViewModel(newVm, newSelection)
  }
})

