import { Paragraph, Selection, Run } from './Paragraph.js'
import CharRuler from './CharRuler.js'

// Split string into lines, each line as long as possible but not exceeding containerWidth
const lineify = (paragraph, containerWidth, ruler) => {
  let cumOffset = 0
  const lines = [{
    paragraph: paragraph,
    paragraphOffset: cumOffset,
    spans: [],
  }]

  for (let run of paragraph.runs) {
    const words = run.text.split(/(\s+)/g)
    const currentLine = lines[lines.length-1]

    currentLine.spans.push({ text: '', formats: run.formats })

    for (let word of words) {
      const spans = lines[lines.length-1].spans
      const currentSpan = spans[spans.length-1]

      // Text of line if we decide to put the next word on, and its px width. We DON'T
      // want to measure white-space that falls at the end of a line, hence the trim().
      const lineTextNew = spans.reduce((acc, sp) => acc + sp.text, '') + word
      const lineWidthNew = ruler.measureString(lineTextNew.trim())

      // TODO: measure width of runs independently and add up their results instead of measuring whole line string at once.
      // This will both be more efficient and also allow us to acount for bolding/italics/format differences in measurement.

      if (lineWidthNew < containerWidth) {
        currentSpan.text += word
      }
      else {
        // No more space on line, make new one
        cumOffset += lineTextNew.length
        lines.push({
          paragraph: paragraph,
          paragraphOffset: cumOffset,
          spans: [{ text: word, formats: run.formats }],
        })
      }
    }
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
