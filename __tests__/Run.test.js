const { Run, Selection } = require('../Paragraph.js')

let run1, run2

beforeAll(() => {
  run1 = new Run(1, 'Foobar')
  run2 = new Run(1, 'Foo')
})

describe('insert', () => {

  // 'Foobar'.insert(1, 3, 'a') -> 'Fabar'
  test('insert with range selection', () => {
    const r = run1.insert('a', 1, 3)
    expect(r.text).toBe('Fabar')
  })


  // 'Foobar'.insert(0, 6, 'Fizzbuz') -> 'Fizzbuzz'
  test('insert when range is whole run', () => {
    const r = run1.insert('Fizzbuzz', 0, 6)
    expect(r.text).toBe('Fizzbuzz')
  })

  // 'Foo'.insert(0, 'h') -> 'hFoo'
  test ('single-selection insert at start of run', () => {
    const r = run2.insert('h', 0)
    expect(r.text).toBe('hFoo')
  })

  // 'Foo'.insert(1, 'h') -> 'Fhoo'
  test('single-selection insert in middle of run', () => {
    const r = run2.insert('h', 1)
    expect(r.text).toBe('Fhoo')
  })

  // 'Foobar'.insert(6, 'a') -> 'Foobara'
  test('single-selection insert at end of run', () => {
   const r = run1.insert('a', 6)
   expect(r.text).toBe('Foobara')
  })

  // 'Foobar'.insert(7, 'a') -> Error
  test('Illegal insert out of range of run', () => {
    expect(() => {
      run1.insert('whatever', 7)
    }).toThrow()
  })

  // 'Foobar'.insert(-1, 7, 'whatever') -> Error
  test('Illegal insert both before and after run', () => {
    expect(() => {
      run1.insert('whatever', -1, 7)
    }).toThrow()
  })

})

describe('remove', () => {

  // 'Foobar'.remove(0, 1) -> 'oobar'
  test('Range remove starting at beginning', () => {
    const r = run1.remove(0, 1)
    expect(r.text).toBe('oobar')
  })

  // 'Foobar'.remove(1, 3) -> 'Fbar'
  test('Range remove starting from middle', () => {
    const r = run1.remove(1, 3)
    expect(r.text).toBe('Fbar')
  })

  // // 'Foobar'.remove(0, 6) -> ''
  test('Remove entire run', () => {
    const r = run1.remove(0, 6)
    expect(r.text).toBe('')
  })

  // 'Foobar'.remove(0) -> Error
  test('Backspace at start', () => {
    expect(() => {
      run1.remove(0)
    }).toThrow()
  })

  // 'Foobar'.remove(5) -> 'Foobr'
  test('Backspace in middle of selection', () => {
    const r = run1.remove(5)
    expect(r.text).toBe('Foobr')
  })

  // 'Foobar'.remove(6) -> 'Fooba'
  test('Backspace at end', () => {
    const r = run1.remove(6)
    expect(r.text).toBe('Fooba')
  })

  test('Backspace off end of range and before beginning', () => {
    expect(() => {
      run1.remove(7)
    }).toThrow()

    expect(() => {
      run1.remove(-1)
    }).toThrow()

    expect(() => {
      run1.remove(-1, 100)
    }).toThrow()
  })
})

test('applyFormats', () => {
  const newRun = run1.applyFormats(['bold', 'italic']).applyFormats(['bold', 'underline'])
  expect(newRun.formats.sort()).toEqual(['bold', 'italic', 'underline'].sort())
})

test('removeFormats', () => {
  const newRun = run1.applyFormats(['bold', 'italic']).removeFormats(['italic', 'bold'])
  expect(newRun.formats).toEqual([])
})

describe('split', () => {
  const myRun = new Run(1, 'firstsecond')

  test('middle', () => {
    const [split1, split2] = myRun.split(5)
    expect(split1.text).toBe('first')
    expect(split2.text).toBe('second')
  })

  test('start', () => {
    const [split1, split2] = myRun.split(0)
    expect(split1.text).toBe('')
    expect(split2).toEqual(myRun)
  })

  test('end', () => {
    const [split1, split2] = myRun.split(myRun.length)
    expect(split1).toEqual(myRun)
    expect(split2.text).toBe('')
  })
})
