# Agnostic library for building Forms

Provides a set of configuration options for building forms components. Here's a list of available components:
- Input text
- Textarea
- Select
- Radio 
- Checkbox
- Datepicker
- Autocomplete

# Configuring fields

You can only reconstruct the original (opaque) type:

`type FormField model msg
    = FormField (FormFieldConfig model msg)`

By using one of this configuration methods:
- `autocompleteConfig`
- `checkboxConfig`
- `datepickerConfig`
- `radioConfig`
- `selectConfig`
- `textConfig`

Once configured, a field must be rendered by calling the `render` method.


# Use example
See `examples/FormApp.elm`
