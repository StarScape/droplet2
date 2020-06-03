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
    this.element.innerHTML = ''

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
    this.element.innerHTML = ''

    return this.characterDiffs[chars]
  }

  measureString(str) {
    // const start = this.measure(str[0])
    let sum = this.measure(str[0])

    for (let i = 1; i < str.length; i++) {
      const prev = str[i-1]
      const curr = str[i]

      sum += this.measureDiff(prev, curr)
    }

    return sum
  }
}

// Split string into lines, each line as long
// as possible but not exceeding containerWidth
const lineify = (str, containerWidth, fontSize, fontFamily) => {
  const ruler = new CharRuler(fontSize, fontFamily)
  const words = str.split(' ')
  const lines = [{ text: words[0], width: ruler.measureString(words[0]) }]
  let line = 0

  for (let word of words.slice(1)) {
    const fullWord = ' ' + word
    const speculativeLineText = lines[line].text + fullWord
    const speculativeLineWidth = ruler.measureString(speculativeLineText)

    if (speculativeLineWidth < containerWidth) {
      lines[line].text += fullWord
      lines[line].width = speculativeLineWidth
    }
    else {
      lines[++line] = {
        text: word,
        width: ruler.measureString(fullWord),
      }
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

const lines = lineify(str, 200, fontSize, fontFamily)

for (let l of lines) {
  const el = document.createElement('div')
  el.innerHTML = l.text
  fakeEditor.appendChild(el)
}
