/**
 * Class for measuring the widths characters will take up inside an element,
 * both by themselves, and after other characters. This is to account for
 * rendering effects such as kerning or scaling.
 *
 * Measurements are made by capturing the width of a hidden DOM element, but
 * a cache of these values is maintained in the background, so subsequent calls
 * will be cheaper. (This is also why CharRuler is implemented as a class, rather
 * than a few pure functions, though the methods of a CharRuler are essentially pure).
 */
class CharRuler {
  /**
   * Create a CharRuler for a given font and font size.
   * @param fontSize {string} - Font size to measure with. Any valid CSS 'font-size' value.
   * @param fontFamily {string} - Font family to measure with. Any valid CSS 'font-family' value.
   */
  constructor(fontSize, fontFamily) {
    this.singleCharacterWidths = {}
    this.characterDiffs = {}

    this.fontSize = fontSize
    this.fontFamily = fontFamily

    this.element = document.createElement('div')
    this.element.style.visibility = 'hidden'
    this.element.style.position = 'absolute'
    this.element.style.whiteSpace = 'pre'
    this.element.style.margin = 0
    this.element.style.padding = 0
    this.element.style.fontSize = fontSize
    this.element.style.fontFamily = fontFamily

    document.body.appendChild(this.element)
  }

  /**
   * Measures the width a single character takes up. Results are cached.
   *
   * @param {string} char - Single character to measure.
   * @return {number} Width of character, in pixels.
   */
  measure(char) {
    if (this.singleCharacterWidths[char]) {
      return this.singleCharacterWidths[char]
    }

    this.element.innerHTML = char
    this.singleCharacterWidths[char] = this.element.getBoundingClientRect().width
    // this.singleCharacterWidths[char] = this.element.clientWidth
    // this.element.innerHTML = ''

    return this.singleCharacterWidths[char]
  }

  /**
   * Measures how much ADDITIONAL space char2 will take up, assuming
   * char1 immediately preceeds it. This is to account for kerning effects.
   * Results are cached.
   *
   * @param {string} char1 - First char
   * @param {string} char2 - Char immediately following it
   *
   * @return {number} Width added by char2, in pixels.
   */
  measureDiff(char1, char2) {
    if (char2 === null || char2 === undefined) {
      throw new Error("char2 must be supplied to measure diff")
    }
    const chars = char1 + char2

    if (this.characterDiffs[chars]) {
      return this.characterDiffs[chars]
    }

    this.element.innerHTML = chars
    this.characterDiffs[chars] = this.element.getBoundingClientRect().width - this.measure(char1)
    // this.characterDiffs[chars] = this.element.clientWidth - this.measure(char1)
    // this.element.innerHTML = ''

    return this.characterDiffs[chars]
  }

  _measureString(str) {
    let sum = this.measure(str[0])

    for (let i = 1; i < str.length; i++) {
      const prev = str[i-1]
      const curr = str[i]

      sum += this.measureDiff(prev, curr)
    }

    return sum
  }

  measureString(str) {
    let sum = this.measure(str[0])

    for (let i = 1; i < str.length; i++) {
      const prev = str[i-1]
      const curr = str[i]

      // sum += this.measureDiff(prev, curr)
      sum += this.measure(curr)
    }

    return sum
  }
}

// Split string into lines, each line as long as possible but not exceeding containerWidth
const lineify = (str, containerWidth, ruler) => {
  const words = str.split(/(\s+)/g)
  const lines = [{ text: '' }]

  for (let word of words) {
    // Text of line if we decide to put the next word on, and its length. We DON'T
    // want to measure white-space that falls at the end of a line, hence the trim().
    const lineTextNew = lines[lines.length-1].text + word
    const lineWidthNew = ruler.measureString(lineTextNew.trim())

    if (lineWidthNew < containerWidth) {
      lines[lines.length-1].text += word
    }
    else {
      lines.push({
        text: word,
      })
    }
  }

  return lines
}

const fakeEditor = document.getElementById('fake-editor')
const style = getComputedStyle(fakeEditor)
const fontSize = style.getPropertyValue('font-size')
const fontFamily = style.getPropertyValue('font-family')

const ruler = new CharRuler(fontSize, fontFamily)
const str = "Hello world, this is an example of a paragraph that I might want to split into lines. I'm really just typing a bunch of random stuff in here. Don't know what else to say. Hmmmm..."
const str2 = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
const str3 = "The first publicly released version of Arc was made available on 29 January 2008,[10] implemented on Racket (named PLT-Scheme then). The release comes in the form of a .tar archive, containing the Racket source code for Arc. A tutorial[11] and a discussion forum[12] are also available. The forum uses the same program that Hacker News does, and is written in Arc."

const lines = lineify(str, 200, ruler)

for (let l of lines) {
  const el = document.createElement('div')
  el.innerHTML = l.text
  fakeEditor.appendChild(el)
}
