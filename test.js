import { Paragraph, Selection, Run } from './Paragraph.js'

let selection = new Selection({ pid: 1, offset: 0 })
let paragraph = new Paragraph()

const syncParagraphWithDom = (elem, paragraph, selection) => {
  elem.innerHTML = paragraph.render()
}


window.onload = () => {
  const editor = document.querySelector('#editor')

  editor.addEventListener('input', (e) => {
    // console.log('input')
    // console.log(e);
    e.preventDefault()
  })

  // TODO: handle prevent input and using our editor HERE
  editor.addEventListener('beforeinput', (e) => {
    e.preventDefault()

    if (e.inputType === 'insertText') {
      const [newParagraph, newSelection] = paragraph.insert([new Run(e.data, ['italic'])], selection)

      paragraph = newParagraph
      selection = newSelection

      syncParagraphWithDom(editor, paragraph, selection)
    }
  })

  editor.addEventListener('keydown', (e) => {
    // console.log(e);

    if (e.key === 'i' && e.ctrlKey === true) {
      e.preventDefault()
    }
  })
}