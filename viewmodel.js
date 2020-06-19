import { Paragraph, Selection, Run } from './Paragraph.js'
import CharRuler from './CharRuler.js'

// Split string into lines, each line as long as possible but not exceeding containerWidth
const lineify = (paragraph, containerWidth, ruler) => {
  let widthOfPrevSpans = 0
  let cumCharOffset = 0
  const lines = [{
    paragraph: paragraph,
    paragraphOffset: cumCharOffset,
    spans: [],
  }]

  for (let run of paragraph.runs) {
    const words = run.text.split(/(\s+)/g)
    lines[lines.length-1].spans.push({ text: '', formats: run.formats, width: 0 })

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
        const lineTextNew = spans.reduce((txt, sp) => txt + sp.text, '')
        cumCharOffset += lineTextNew.length

        widthOfPrevSpans = 0

        lines.push({
          paragraph: paragraph,
          paragraphOffset: cumCharOffset,
          spans: [{ text: word, formats: run.formats, width: 0 }],
        })
      }
    }

    const currentLine = lines[lines.length-1]
    widthOfPrevSpans += currentLine.spans[currentLine.spans.length-1].width
  }

  return lines
}

const fakeEditor = document.getElementById('fake-editor')
const style = getComputedStyle(fakeEditor)
const fontSize = style.getPropertyValue('font-size')
const fontFamily = style.getPropertyValue('font-family')

// TODO: Test with this exact example
const ruler = new CharRuler(fontSize, fontFamily)
const para = new Paragraph([
  new Run("Hello world, this is an example of a paragraph ", []),
  new Run("that I might want to split into lines. I'm really just typing a bunch of random stuff in here. ", ['italic']),
  new Run("Don't know what else to say. Hmmmm...", ['bold'])
])

const lines = lineify(para, 200, ruler)
// console.log(lines);

for (let line of lines) {
  const lineElem = document.createElement('div')

  for (let span of line.spans) {
    const spanElem = document.createElement('span')
    spanElem.innerHTML = span.text

    // Apply style
    for (let format of span.formats) {
      if (format === 'bold') {
        spanElem.style.fontWeight = 'bold'
      }
      else if (format === 'italic') {
        spanElem.style.fontStyle = 'italic'
      }
      // TODO: rest of formats
    }

    lineElem.appendChild(spanElem)
  }

  fakeEditor.appendChild(lineElem)
}
