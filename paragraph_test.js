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

const caretDown = (viewmodel, selection) => {
  // TODO
}

const renderCaret = () => {
  return `<span class='text-caret'></span>`
}

// TODO: refactor this to return a string, and an empty string if the span is empty?
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
        const [span1, span2] = span.split(spanOffset).map(s => renderSpan(s))
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
    selection = selection.shiftSingle(-1)
  }
  else if (e.code === 'ArrowRight' && selection.caret < paragraph.length) {
    selection = selection.shiftSingle(1)
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

