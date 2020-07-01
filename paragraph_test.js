import { Paragraph, Selection, Run, Selection as MySelection } from './Paragraph.js'
import { ParagraphViewModel } from './ViewModel.js'
import CharRuler from './CharRuler.js'

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

// Measure list of spans
const measureSpans = (spans, ruler) => {
  return spans.reduce((width, sp) => width + sp.measure(ruler), 0)
}

const caretDown = (viewmodel, selection, ruler) => {
  const lineIdx = getCaretLine(viewmodel, selection)

  // TODO: Clean all this up, think of a more elegant way.
  if (lineIdx < viewmodel.lines.length-1) {
    const beforeCaret = viewmodel.lines[lineIdx].split(selection.caret)[0]
    const caretPixelOffset = measureSpans(beforeCaret, ruler)
    const nextLine = viewmodel.lines[lineIdx + 1]
    const nextLineEnd = nextLine.offset + nextLine.length

    let newOffset = nextLine.offset
    let newPixelOffset = 0
    let lastPixelOffset = 0

    for (let span of nextLine.spans) {
      for (let char of span.text) {
        if (newPixelOffset >= caretPixelOffset) {
          if (Math.abs(lastPixelOffset - caretPixelOffset) < Math.abs(newPixelOffset - caretPixelOffset)) {
            return selection.setSingle(newOffset - 1)
          }
          return selection.setSingle(newOffset)
        }

        lastPixelOffset = newPixelOffset
        newOffset++
        newPixelOffset += ruler.measure(char)
      }
    }

    return selection.setSingle(nextLineEnd)
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
  fakeEditor.innerHTML = renderViewModel(viewmodel, selection)
}

document.addEventListener('keydown', (e) => {
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
    // TODO
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

