# Elm Markdown Test Suite

- To run the tests from the command line: `elm-test CLI.elm`;
- To view the tests in the browser:
	- Run `elm-reactor` in **this** folder;
	- Open in the browser `http://localhost:8000/Main.elm`.

## TODO Tests

- 300 in `Test/Inline/Entity.elm`: Limited html entity decoding support, due to compiler issues with large Dicts;
- 465 in `Test/Inline/Link.elm`: No balanced parenthesis in inline link's url support (e.g.: `[link](url() "title")`, use `[link](<url()> "title")` instead).
