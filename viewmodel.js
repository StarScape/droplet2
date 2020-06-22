import { Paragraph, Selection, Run } from './Paragraph.js'
import CharRuler from './CharRuler.js'

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
      paragraphOffset: null,
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
            paragraphOffset: null,
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
      span.paragraphOffset = cumCharOffset
      cumCharOffset += span.length
    }
  }

  // TODO: replace with ViewModel#length
  lines.paragraphLength = paragraph.length

  return lines
}

const moveCaretDown = (viewModel, offset) => {
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

const renderViewModel = (viewModel, caret) => {
  let html = ''

  for (let line of viewModel) {
    const lineElem = document.createElement('div')

    for (let span of line.spans) {
      const start = span.paragraphOffset
      const end = start + span.length

      if ((caret >= start && caret < end) || (end === viewModel.paragraphLength && caret === end)) {
        const spanOffset = caret - span.paragraphOffset
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
  if (e.code === 'ArrowLeft' && caret > 0) {
    caret--
  }
  else if (e.code === 'ArrowRight' && caret < para.length) {
    caret++
  }

  const lines = lineify(para, 200, ruler)
  fakeEditor.innerHTML = renderViewModel(lines, caret)
})
