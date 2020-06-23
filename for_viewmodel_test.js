// Check that offsets are correct
window.s = "Hello world, this is an example of a paragraph that I might want to split into lines. I'm really just typing a bunch of random stuff in here. Don't know what else to say. Hmmmm..."
const offsets = [0, 32, 47, 65, 102, 136, 142, 166]
let io = 0

const viewmodel = ParagraphViewModel.fromParagraph(para, 200, ruler)
console.log(viewmodel);

for (const line of viewmodel.lines) {
  for (const span of line.spans) {
    console.log(`span text: '${span.text}' at offset: ${span.offset}`);
    if (offsets[io] !== span.offset) {
      throw new Error(`offset: ${offsets[io]}, paragraphOffset: ${span.offset}`)
    }
    io++
  }
}
