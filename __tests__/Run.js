const { Run, Selection } = require('../Paragraph.js')

let run1, run2

beforeAll(() => {
  run1 = new Run(1, 'Foobar')
  run2 = new Run(1, 'Foo')
})

describe('insert', () => {

  // 'Foobar'.insert(1, 3, 'a') -> ['Fabar', 2]
  test('insert with range selection', () => {
    const [r, s] = run1.insert(new Selection({ offset: 1, elem: 1 }, { offset: 3, elem: 1 }), 'a')

    expect(r.text).toBe('Fabar')
    expect(s.start.offset).toBe(2)
    expect(s.single).toBe(true)
  })


  // 'Foobar'.insert(0, 6, 'Fizzbuz') -> 'Fizzbuzz', sel(8)
  test('insert when range is whole run', () => {
    const [r, s] = run1.insert(new Selection({ offset: 0, elem: 1 }, { offset: 6, elem: 1 }), 'Fizzbuzz')

    expect(r.text).toBe('Fizzbuzz')
    expect(s.start.offset).toBe(8)
  })

  // 'Foo'.insert(0, 'h') -> 'hFoo', sel(1)
  test ('single-selection insert at start of run', () => {
    const [r, s] = run2.insert(new Selection({ offset: 0, elem: 1 }), 'h')

    expect(r.text).toBe('hFoo')
    expect(s.start.offset).toBe(1)
  })

  // 'Foo'.insert(1, 'h') -> 'Fhoo', sel(2)
  test('single-selection insert in middle of run', () => {
    const [r, s] = run2.insert(new Selection({ offset: 1, elem: 1 }), 'h')

    expect(r.text).toBe('Fhoo')
    expect(s.start.offset).toBe(2)
  })

  // 'Foobar'.insert(6, 'a') -> 'Foobara', sel(7)
  test('single-selection insert at end of run', () => {
   const [r, s] = run1.insert(new Selection({ offset: 6, elem: 1 }), 'a')

   expect(r.text).toBe('Foobara')
   expect(s.start.offset).toBe(7)
  })

  // 'Foobar'.insert(7, 'a') -> Error
  test('Illegal insert out of range of run', () => {
    expect(() => {
      run1.insert(new Selection({ offset: 7, elem: 1 }), 'whatever')
    }).toThrow()
  })

  // 'Foobar'.insert(-1, 7, 'whatever') -> Error
  test('Illegal insert both before and after run', () => {
    expect(() => {
      run1.insert(new Selection({ offset: -1, elem: 1 }, { offset: 7, elem: 1 }), 'whatever')
    }).toThrow()
  })

})

describe('remove', () => {

  // 'Foobar'.remove(0, 1) -> 'oobar', sel(0)
  test('Range remove starting at beginning', () => {
    const [r, s] = run1.remove(new Selection({ offset: 0, elem: 1 }, { offset: 1, elem: 1 }))

    expect(r.text).toBe('oobar')
    expect(s.start.offset).toBe(0)
    expect(s.single).toBe(true)
  })

  // 'Foobar'.remove(1, 3) -> 'Fbar', sel(1)
  test('Range remove starting from middle', () => {
    const [r, s] = run1.remove(new Selection({ offset: 1, elem: 1 }, { offset: 3, elem: 1 }))

    expect(r.text).toBe('Fbar')
    expect(s.caret).toBe(1)
  })

  // // 'Foobar'.remove(0, 6) -> '', sel(0)
  test('Remove entire run', () => {
    const [r, s] = run1.remove(new Selection({ offset: 0, elem: 1 }, { offset: 6, elem: 1 }))

    expect(r.text).toBe('')
    expect(s.caret).toBe(0)
  })

  // TODO: remove() at start

  // 'Foobar'.remove(5) -> 'Foobr', sel(4)
  test('Backspace in middle of selection', () => {
    const [r, s] = run1.remove(new Selection({ offset: 5, elem: 1 }))

    expect(r.text).toBe('Foobr')
    expect(s.caret).toBe(4)
  })

  // TODO: remove at end

  // TODO: test remove() edge cases and errors
})