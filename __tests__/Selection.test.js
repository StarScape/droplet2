const { Selection } = require('../Paragraph.js')

test('initialization', () => {
  const sel = new Selection({ paragraph: 1, offset: 1 }, { paragraph: 1, offset: 12 })

  expect(sel.start.offset).toBe(1)
  expect(sel.end.offset).toBe(12)

  expect(() =>
    new Selection({ paragraph: 1, offset: -1 })
  ).toThrow()

  expect(() => 
    new Selection({ paragraph: 1, offset: 0 }, { paragraph: 1, offset: -100 })
  ).toThrow()
})

test('single', () => {
  const s1 = new Selection({ paragraph: 1, offset: 1 })
  const s2 = new Selection({ paragraph: 1, offset: 1 }, { paragraph: 1, offset: 2 })
  const s3 = new Selection({ paragraph: 1, offset: 1 }, { paragraph: 2, offset: 1 })

  expect(s1.single).toBe(true)
  expect(s2.single).toBe(false)
  expect(s3.single).toBe(true)
})

test('caret', () => {
  const single = new Selection({ paragraph: 1, offset: 144 })
  expect(single.caret).toBe(144)

  const range = new Selection({ paragraph: 1, offset: 0 }, { paragraph: 1, offset: 33 })
  expect(range.caret).toBe(33)

  const backwards = new Selection({ paragraph: 1, offset: 0 }, { paragraph: 1, offset: 33 }, true)
  expect(backwards.caret).toBe(0)
})

test('moveSingle', () => {
  const s = new Selection({ paragraph: 1, offset: 10 })
  expect(s.moveSingle(5).caret).toBe(15)
  expect(() => s.moveSingle(-100)).toThrow()
})

test('setSingle', () => {
  const s = new Selection({ paragraph: 1, offset: 10 })
  expect(s.setSingle(5).caret).toBe(5)
  expect(() => s.setSingle(-100)).toThrow()
})

test('collapseStart and collapseEnd', () => {
  const s = new Selection({ paragraph: 1, offset: 12 }, { paragraph: 1, offset: 33 })
  const collapsed1 = s.collapseStart()
  const collapsed2 = s.collapseEnd()

  expect(collapsed1.caret).toBe(12)
  expect(collapsed1.single).toBe(true)

  expect(collapsed2.caret).toBe(33)
  expect(collapsed2.single).toBe(true)
})

test('smart collapse', () => {
  const single = new Selection({ paragraph: 1, offset: 10 })
  const range = new Selection({ paragraph: 1, offset: 10 }, { paragraph: 1, offset: 20 })
  const backwards = new Selection({ paragraph: 1, offset: 10 }, { paragraph: 1, offset: 20 }, true)

  expect(single.collapse()).toEqual(single)

  const rangeCollapsed = range.collapse()
  expect(rangeCollapsed.single).toEqual(true)
  expect(rangeCollapsed.caret).toEqual(20)

  const backwardsCollapsed = backwards.collapse()
  expect(backwardsCollapsed.single).toEqual(true)
  expect(backwardsCollapsed.caret).toEqual(10)
})

describe('expandLeft and expandRight', () => {
  const single = new Selection({ paragraph: 1, offset: 10 }, { paragraph: 1, offset: 20 })
  // TODO: I don't want to test these right now, honestly
})
