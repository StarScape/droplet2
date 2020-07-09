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
  // TODO: test / replace with formatsToStyleStr()
  static getFormatPrefix(formats) {
    if (formats.length === 0) {
      return ''
    }
    // TODO: replace this with a hash?
    return [...formats].sort() + '-'
  }

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

    this.element = this._createElement()
    document.body.appendChild(this.element)
  }

  _createElement() {
    const element = document.createElement('div')
    element.style.visibility = 'hidden'
    element.style.position = 'absolute'
    element.style.whiteSpace = 'pre'
    element.style.margin = 0
    element.style.padding = 0
    element.style.fontSize = this.fontSize
    element.style.fontFamily = this.fontFamily
    element.style.fontStyle = 'normal'
    element.style.fontWeight = 'normal'

    return element
  }

  _applyCSS(formats) {
    if (formats.length === 0) {
      return
    }

    for (let format of formats) {
      if (format === 'italic') {
        this.element.style.fontStyle = 'italic'
      }
      else if (format === 'bold') {
        this.element.style.fontWeight = 'bold'
      }
      else {
        // TODO: rest of styles
        throw new Error("Unrecognized format: " + format)
      }
    }
  }

  _removeCSS() {
    this.element.style.fontStyle = 'normal'
    this.element.style.fontWeight = 'normal'
  }

  /**
   * Measures the width a single character takes up. Results are cached.
   *
   * @param {string} char - Single character to measure.
   * @return {number} Width of character, in pixels.
   */

  // TODO: change this to 'measureChar'
  measure(char, formats = []) {
    if (char.length !== 1) {
      throw new Error(`String with length greater than 1 passed to measure(): '${char}'`)
    }

    const charKey = CharRuler.getFormatPrefix(formats) + char
    if (this.singleCharacterWidths[charKey]) {
      return this.singleCharacterWidths[charKey]
    }

    this._removeCSS()
    this._applyCSS(formats)
    this.element.innerHTML = char
    this.singleCharacterWidths[charKey] = this.element.getBoundingClientRect().width

    return this.singleCharacterWidths[charKey]
  }

  measureString(str, formats = []) {
    let sum = 0

    for (let i = 0; i < str.length; i++) {
      sum += this.measure(str[i], formats)
    }

    return sum
  }
}

export default CharRuler
