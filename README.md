# Agnostic library for building Forms

Provides a set of configuration options for building forms components. Here's a list of available components:
- Input text
- Textarea
- Select
- Radio
- Checkbox (with single or multiple choices)
- Datepicker
- Autocomplete

# Configuring fields

You can only reconstruct the original (opaque) type:

`type FormField model msg
    = FormField (FormFieldConfig model msg)`

By using one of this configuration methods:
- `autocompleteConfig`
- `checkboxConfig`
- `checkboxWithOptionsConfig`
- `datepickerConfig`
- `radioConfig`
- `selectConfig`
- `textConfig`

Once configured, a field must be rendered by calling the `render` method.


# Use example
See `examples/FormApp.elm`

# Running example
- Run `elm-reactor`
- Open `http://localhost:8000`
- Navigate to `http://localhost:8000/examples/FormApp.elm`
