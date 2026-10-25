# Design specification for devoirs3

## Purpose

`{devoirs3}` will be an R package intended mostly for use within Quarto `.qmd` documents in order to set up spans or divs that each contain a mechanism for students to select or compose answers to a question posed using markdown outside the span or div.

I'll refer to several different human roles and modes for the document.

1. The "author" is the composer of the `.qmd` file who writes narrative and figures and other typical content for a `.qmd` file and inserts, at selected points in that typical content, a function call to create interactive elements.

2. The "student" is the target reader for the document in an HTML format generated from the `.qmd`. When viewing the HTML version in a browser, the student can select from a menu or set of radio buttons or enter text.

3. The "source document" is in `.qmd` form.

4. The "target document" is in HTML form. It will be posted on a web site and will work without any back-end server. The interactive functionality is to be provided by javascript, CSS, or perhaps webR functionality.

5. An "answer selector" is an integrated set of HTML elements through which the student can select from a set of answers provided by the author, reset the selection to a null initial choice, and see some author-generated feedback associated to be displayed when the student makes a selection.

6. A "text entry" is an HTML text-entry box of whatever specified size, either as a div or a span, where the student can enter or edit some text. 

7. An "question element" is a general term that can refer either to an answer selector or a text entry.

8. The "collector" is an HTML button which, upon clicking, collates all the selections and text from the answer-elements and pastes a JSON array with this information into the student's clipboard.

9. The "collection site" is a facility such as a Google Form where students will submit the JSON array placed on the user clipboard by the collector. The construction and management of the collection site is outside of the scope of the facilities provided by `{devoirs3}`.

10. Feedback toggle. A control set in the document .qmd sources to control whether the feedback from selection items should be displayed upon choice of that item. This should be reconfigurable from question item to question item so that feedback display can be turned on for some question items and off for others.

11. Label field. Each question element will have a short text-display span whose contents are initially blank. If the document-level "labels?" checkbox is checked, all the label fields should show the "label" of their respective question element.

12. Feedback field. Each question element will have a text display span or div whose contents are initially blank. If the feedback toggle is on, the feedback string for a selected item should be displayed. For answer-elements designated as "inline" the feedback field should be a div, otherwise a span. The behavior of the feedback field for `devoirs3::devoirs_text()` answer choices is not yet defined. That will happen in a later version of this specification.

13. Each of the choices in an answer selector has an automatically computed `optionId`, unique within that question. The `optionId` will be stable across renders by virtue of the random key set by `rolling_hash(label)`. Choice correctness is author-supplied data used for immediate feedback. It is not included in the submission, but it is embedded in the `optionId`. Correctness is not a separate field in the submission; it is embedded in the `optionId`. Because the rendered HTML and JavaScript are available to students, correctness information cannot be kept secret in an offline document; this package does not provide tamper-proof grading or cheating detection.


The package will provide a set of R functions with names like `devoirs_selection()`, `devoirs_text()`, `devoirs_start()` among others, that construct the question elements in the rendered HTML document. I'll outline the desired behavior using `devoirs_selection()` and `devoirs_text()`, which have many similarities.

## Example `{devoirs3}` source document: example.qmd

````{verbatim}
# My example document

```{r include=FALSE}
library(devoirs3)
devoirs3::devoirs_start(docID="wednesday", feedback=TRUE, collector=TRUE)
```

Consider a few planets and satellites. 

```{r}
# When only one answer is correct, use radio buttons.
Q1 <- devoirs_selection(
  label = "D10-satellite",
  question = "Which one is a satellite?",
  answers = list(
    list(text = "Mars", correct = FALSE,
      feedback = "Sorry, it's a satellite of the Sun."),
    list(text = "Earth", correct = FALSE,
      feedback = "We live here."),
    list(text = "Sun", correct = FALSE,
      feedback = "Bright, but not right!"),
    list(text = "Moon", correct = TRUE,
      feedback = "Right! Not big enough to be a planet.")
  )
)
```


```{r}
# When multiple answers are correct, use checkboxes.
Q2 <- devoirs_selection(
  label = "D10-planets",
  question = "Which one(s) are planets?",
  multiple = TRUE,
  answers = list(
    list(text = "Mars", correct = TRUE,
      feedback = "Good!"),
    list(text = "Earth", correct = TRUE,
      feedback = "Our favorite planet."),
    list(text = "Sun", correct = FALSE,
      feedback = "Bright, but not right!"),
    list(text = "Moon", correct = FALSE,
      feedback = "Some people think it should be promoted.")
  )
)
```

A. Which one is a satellite? `r Q1(inline=TRUE, feedback=FALSE)`

B. Which one(s) are planets? `r Q2(inline=TRUE)`

C. Would you like to live on the Moon? Explain in detail why or why not.

`r devoirs_text(label = "D10-moon-live", question = "Would you like to live on the Moon? Explain why or why not.", height=5, width=80)`

Thanks for participating!
````

This example is very small. The system should be able to handle documents with thousands of question elements.

## `devoirs_start()`

Creates the answer collector button with the appropriate click behavior. Next to the collector button will be a checkbox "Labels?" which turns the display of the answer-item labels near the corresponding answer choice.

`devoirs_start()` also creates a "Clear saved answers" button. Activating it clears saved values and durations for this `docID` after confirmation.

Arguments:

- docID : sets the character string ID for the document (default: qmd file name)
- feedback: sets document default on whether to display feedback (default: TRUE)
- collector: whether to construct the collector button (default: TRUE)
- labels: whether to display the "labels?" checkbox. (default: TRUE)

In addition to creating the answer collector button and the "Labels?" checkbox, `devoirs_start()` will insert all the JavaScript needed for the feedback display and answer collection.

`docID` must be unique for each assignment published on the same site origin. It namespaces browser-saved answers and is included in submissions as `docID`. Saved answers use browser `localStorage` under keys scoped by schema version, `docID`, and question label. They are local to the student's browser profile and remain until cleared by the student or browser. Shared devices may expose saved answers to later users. Directly opening rendered HTML with a `file:` URL is not supported; serve the document from a web origin.

### Text entry: `devoirs_text()`

Creates a *function* that can be called with arguments about layout and item-specific feedback control. Invoking the function will insert the text-entry controls into the html target.

#### Arguments:

1. label
: a fixed, non-empty character string that must be unique across question elements in the document. Duplicate labels cause rendering to fail with an error.
: all selectors and text entries will have such a label.
: the label will be used as a unique identifier for each of the question elements in the JSON array to be assembled by the collector. The "label" argument for all the R functions creating question elements will have this function.

2. question
: a human-readable question prompt used as the accessible name for the text-entry control. It may match nearby narrative text; it need not be displayed a second time.

3. placeholder
: a short word or phrase to appear in the text-entry box that will be replaced by whatever the student types.

4. width in characters

5. height in lines

6. inline
: if TRUE, place the text-entry box in an HTML span. Default: FALSE, meaning to place the text-entry box in an HTML div. 

#### Rendered document appearance

A resizable HTML text-entry box. Its label field is initially hidden and can be
shown by the document-level labels control.

#### Behavior

While viewing the HTML document, the student can enter text into the box. Text is saved in browser `localStorage` and restored after reload or browser restart, subject to browser storage availability and the student's browser profile. Line endings are normalized to LF when collected.

The collector will put the contents of the box (if any) and the label into the JSON array. If the box is empty, don't collect it.

Text-entry duration is the cumulative number of seconds that the field is focused while the document is visible. Timing pauses on blur or when the document becomes hidden, resumes on refocus, and is included in the submission when collected. The accumulated duration is saved with the text and survives reloads. Resetting a text entry clears its value and accumulated duration.


### Selector: `devoirs_selection()`

Creates a *function* that can be called with layout and item-specific feedback control. Invoking the created function will insert the selector elements into the html target.


#### Arguments: 

The arguments will be 

label
: As described previously.

question
: A human-readable question prompt used as the accessible name for the radio-button or checkbox group. It may match nearby narrative text; it need not be displayed a second time.

answers
: A non-empty list of choice records. Each record has a  unique-within-question `optionId`, display `text`, logical `correct`, and optional `feedback` string. The author supplies the `text`, `correct`, and `feedback` values. If feedback is omitted, use "Correct!" for correct choices and "Sorry." for incorrect choices. Choice text may contain inline LaTeX delimiters for MathJax rendering; arbitrary HTML is not supported. The `optionId` will be assigned automatically. See the section headed `optionId`.

multiple
: If `FALSE` (default), render a radio-button group and allow exactly one choice. If `TRUE`, render checkboxes and allow one or more choices.

The `devoirs_selection()` call returns a render function. That function accepts `inline` (default `FALSE`) and `feedback` (default: inherit the `devoirs_start()` setting) to control layout and feedback for that use of the question. The question prompt remains available to assistive technology but is not displayed a second time; block choices are placed on separate lines.

Choices are rendered in the order supplied by the author; version 1 does not randomize them. The optional `inline` argument controls inline versus block layout, and the optional question-level `feedback` argument overrides the document default.

See the example document for typical usage of `devoirs_selection()`.

#### `optionId`

Each choice record has a character string `optionId`. This consists of two capital letters followed by five randomly selected letters. If the choice is correct, the five "random" letters should be overwritten to have only one vowel or three vowels, not counting the initial capital letter. If the choice is incorrect, there should be zero or two vowels. Vowels are defined as a, e, i, o, and u.

The initial capital letter will reflect the position of the choice record in the list of records. The first position gets "AA," the second "BA," and so on. Should there be more than 26 choices, change the second capital letter from "A" to "B" and so on, so that the 27th choice will be "AB" and the 28th choice will be "BB". 

The `optionId` will be stable between renders of the .qmd to HTML. This is accomplished by using the author-specified selection label to generate the random seed for constructing the five-character suffix according to the "Polynomial Rolling Hash with Modulo" algorithm implemented in `R/rolling_hash.R`.


#### Function 
The feedback field is displayed after a choice is added if feedback is enabled for that question. Deselecting a choice in a checkbox set should blank out the displayed feedback content. A question-level `feedback` argument overrides the document-level default set by `devoirs_start()`; otherwise the document default applies. For multiple-choice questions, show feedback for the most recently changed choice. Correctness and feedback are present in the client-side document and are not secure from inspection.


#### Rendering

Duplicate labels for question elements should be detected during rendering of the qmd to HTML. When a duplicate is encountered, the render should stop with an error and a message like: "Label <yhs> is duplicated. All labels must be unique within the document."

The choices will be displayed with a radio button or checkbox (depending on `multiple`), each associated with its text label. Choice text is treated as text plus supported math delimiters, not as arbitrary HTML.

A small "clear" button should be provided with each selector question element. 

#### Behavior

The student can select one or more choices as allowed by `multiple`. When the selection changes, the author-designated feedback for the changed choice is displayed if feedback is enabled.

The student's selection(s), if any, are saved in browser `localStorage` and restored after reload or browser restart, subject to browser storage availability and the student's browser profile.

Upon pressing the "clear" button, all choices for that question are deselected and its saved response and feedback are cleared. Focus remains on the clear button.

## Answer collection

At the top of the document, with a spatially compact but conspicuous layout, will be a button to trigger collection of the student's answers thusfar.

- Only items for which a selection has been made (and not reset) or text entered should be collected.

- The collected items will be packaged into a JSON object that will be serialized as a JSON string and copied to the student's clipboard. As backup to clipboard access, the JSON string is displayed in a read-only text area so the student can copy it manually. The area shows one line by default and can be resized.

- It is the student's responsibility to paste the JSON string into a collection site that is not part of the document. Typically, the collection site will be a Google Form set up by the instructor with a field to copy the JSON to and identity validation performed by the Form itself, not by the document.

Presumably the operation of the collector will be implemented in JavaScript or some other language that runs in the browser. All JavaScript should be contained within the html file, placed there by the `devoirs_start()` function, without creating an external dependency.

The necessarily JavaScript should also be placed into a static file under `inst/` so that it can readily be accessed by `devoirs::start()` to insert it into the html document.

### Information to collect

When the student presses the collector button, a submission, described in the "JSON Schema for submissions" section, is to be assembled. 

- `docID` is the value specified in the `devoirs_start()` call.
- `collectedAt` is the UTC time when collection is triggered, in ISO 8601/RFC 3339 form with millisecond precision.

The "responses" in the schema should be constructed by polling the question elements, including only those for which a selection has been made or text entered.

- `label` is the question label.
- `type` is either `text` or `selection`.
- `value` is the entered string for a text question or an array of selected stable option IDs for a selection question.
- `duration` is included only for text responses and follows the focus-time definition above.
- A selection question produces exactly one response object, even when it has multiple selected option IDs. The order of IDs in `value` follows the author's choice order, not the order in which the student selected them.

Correctness is not included in submissions. Instructors grade option IDs against the answer key in their source document. The package makes no claim that client-side answers, timestamps, or responses are tamper-proof.


#### JSON Schema for submissions

```
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "devoirs3 submission",
  "type": "object",
  "required": ["schemaVersion", "docID", "collectedAt", "responses"],
  "properties": {
    "schemaVersion": { "const": 1 },
    "docID": { "type": "string", "minLength": 1 },
    "collectedAt": {"type": "string", "format": "date-time" },
    "responses": {
      "type": "array",
      "items": {
        "oneOf": [
          {
            "type": "object",
            "required": ["label", "type", "value", "duration"],
            "properties": {
              "label": { "type": "string", "minLength": 1 },
              "type": { "const": "text" },
              "value": { "type": "string", "minLength": 1 },
              "duration": {"type": "number", "minimum": 0 }
            },
            "additionalProperties": false
          },
          {
            "type": "object",
            "required": ["label", "type", "value"],
            "properties": {
              "label": { "type": "string", "minLength": 1 },
              "type": { "const": "selection" },
              "value": {
                "type": "array",
                "minItems": 1,
                "uniqueItems": true,
                "items": { "type": "string", "minLength": 1 }
              }
            },
            "additionalProperties": false
          }
        ]
      }
    }
  },
  "additionalProperties": false
}
```

Example submission for the example document above, after selecting Moon, selecting Mars and Earth, and entering text:

```json
{
  "schemaVersion": 1,
  "docID": "wednesday",
  "collectedAt": "2026-10-25T15:30:12.345Z",
  "responses": [
    {
      "label": "D10-satellite",
      "type": "selection",
      "value": ["DAobht"]
    },
    {
      "label": "D10-planets",
      "type": "selection",
      "value": ["AAajoet", "BAurdsg"]
    },
    {
      "label": "D10-moon-live",
      "type": "text",
      "value": "Line one\\nLine two",
      "duration": 12.34
    }
  ]
}
```

#### Encoding for submissions


Unicode: Serialize the JSON as UTF-8, without a byte-order mark. Keep characters such as é and — as Unicode; there’s no need to convert them to \u escapes. JSON parsers handle both forms.

Newlines: Normalize student-entered line endings to LF (\n) when collecting text. JSON serialization represents those line breaks with the escape sequence \n; the parsed value contains actual line breaks. Don’t HTML-encode answer text before putting it in JSON.


## HTML controls

For these widgets, favor native HTML controls and predictable browser behavior over custom keyboard handling.

Keyboard behavior: Keep controls in document reading order. Use native radio buttons when `multiple=FALSE` and native checkboxes when `multiple=TRUE`; Space selects or toggles the focused choice. Each checkbox is a tab stop; native radio groups use standard browser radio-group keyboard behavior. Text entry supports normal editing keys. Clear and collect controls are real buttons, operable with Enter and Space. Avoid keyboard traps and custom shortcuts that intercept typing or browser commands.

Accessible names: Use the `question` argument as the accessible name for each text field and selection group; it may be visually hidden when the same prompt is already present in the surrounding document. Group each selection question in a `<fieldset>` with a `<legend>` and associate each input with its visible choice text using a `<label>`. Placeholders are not labels. Give clear buttons a question-specific accessible name and name the collector button “Copy answers.” Use `aria-describedby` for additional instructions, not as a substitute for a control's name.

Focus behavior: Don’t move focus when a student makes a selection or types. After activating clear, leave focus on that button. Announce collection success or failure through a polite status region without moving focus. If clipboard access fails, reveal the copyable text area, announce the fallback, and move focus to it so keyboard users can copy the submission.

Feedback can be announced with a polite live region, for example `role="status"` and `aria-live="polite"`. Keep it out of the tab order and avoid unnecessarily repetitive announcements.

A CSS inline include will specify the color, background color, and text style of the feedback display field for each question element. The CSS specification should be written to a file inst/widget.css, whence it will be copied into the HTML file. Here are some initial ideas.

1. feedback display field. Should be framed and the displayed text should be in blue color.
2. clear button. Should be shaded light gray and the button label should be in 6pt font size.
3. selection options: there should be enough space between selection options that it will be obvious which radio button corresponds to which text display. Use 1em between options for the inline format. The newline suffices for the non-inline format.
4. labels display: these should show up as a gray color in italic font.

## Chunk engines

In order to avoid complicated nested lists in lists and quotation marks various chunk engines will allow plain text YAML to be used in a specialty chunk.

### Choice-data chunk engine: `devoirs_choices`

A `{devoirs_choices}` chunk defines choice data in YAML and makes it available to later R chunks. Its chunk label is the name of the R variable receiving the choice list.

```{devoirs_choices}
#| label: forQ1
- "True":
    correct: true
    feedback: "Correct!"
- "False"
```

Another example:

```{devoirs_choices}
#| label: fred
- "TRUE":
    correct: true
    feedback: "Fantastic!"
- "False"
```

Use the resulting variable with the existing `answers` argument, e.g.:

```{r}
Q1 <- devoirs_selection(
  label = "question-1",
  question = "Is this statement true?",
  answers = forQ1
)
```

`devoirs_selection()` continues to control question behavior and rendering; the choices chunk only defines its answer data.

#### Contract

- The chunk label is required, must be a valid R variable name, and must be unique in the document.
- The chunk body is YAML data only; it is not evaluated as R code and produces no visible output.
- The YAML root must be a non-empty sequence. Each sequence item defines one choice.
  - A string item is the displayed text; it defaults to `correct: false` and feedback `"Sorry."`.
  - A mapping item has exactly one key: the displayed choice text. Its value is a mapping with optional `correct` and `feedback` fields.
  - `correct` must be a YAML boolean and defaults to `false`. Omitted feedback defaults to `"Correct!"` or `"Sorry."` based on correctness; explicit feedback overrides the default.
  - Unknown metadata fields and malformed items are errors. Explicit feedback, including an empty string, overrides the default.
- Choice text and feedback are plain text with supported MathJax delimiters, not arbitrary HTML. Quote YAML strings when necessary to ensure they are parsed as strings; use block scalars for multiline text.
- The engine assigns the resulting list to the variable named by the chunk label in the document’s R evaluation environment, making it available to subsequent chunks.
- Invalid YAML or invalid choice data must stop rendering with an error that identifies the chunk and, when applicable, the choice number.
- The chunk label names the R variable only; question labels and stable option IDs continue to follow the `devoirs_selection()` contract.

## Plan of work

1. **Finalize the contract (complete):** automatically generated option IDs; explicit option text, correctness, and feedback; one response per question; `docID` namespacing; localStorage persistence and clearing; focus-time duration; and a versioned submission schema. Correctness of an option in the JSON collection is obfuscated, shown by the vowel count in the otherwise randomly generated 5-character suffix.

2. **Scaffold and test the package (complete):** create the package metadata, license, namespace, and test setup. Add a smoke test that builds/renders a minimal Quarto document such as the example .qmd file listed above.

3. **Construct an example .qmd file (complete):** in the inst/example directory. Pull it from the example document in design-spec.md.

4. **Implement `devoirs_text()` end to end:** including markup, persistence, reset, and focused tests. Agree on the HTML attributes/IDs that the JavaScript will rely on.

5. **Implement `devoirs_start()`(complete):** 

6. **Implement `devoirs_selection()`:**

7. **Implement the `devoirs_choices` chunk engine (complete):** parse YAML choice data, normalize it to the `devoirs_selection()` answer contract, and assign it to the document environment.

8. **Add the collector:** serialize according to the finalized schema, handle clipboard failure and the read-only fallback, and test empty and populated responses.

9. **Finish integration and documentation:** test a rendered Quarto HTML file in a browser, including keyboard and screen-reader behavior, reload persistence, duplicate labels, and the collection-site workflow.


