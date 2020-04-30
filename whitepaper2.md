# What is content?
* Text (with formatting *always* associated with it - in other words, a run)
* Text with different formatting applied to different parts - a list of runs
* A paragraph - can be considered identical to the one above
* A list of paragraphs

# Document
insert(content: Array<Run>, selection: Selection) 

# Paragraph
insert(content: Array<Run>, selection: Selection)
insert(content: Run, selection: Selection)

# Run

insert(text: string, start: num, end?: num)

```
State = {
  Selection: {
    orientation: backwards | forwards,
    start: {
      paragraph,
      offset
    },
    end: {
      paragraph,
      offset
    }
  },

  Document: {
    paragraphs: [
      ...
      Paragraph: {
        runs: [
          ...
          Run {

          }
          ...
        ]
      }
      ...
    ]
  }
}
```