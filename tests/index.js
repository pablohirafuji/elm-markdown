'use strict';

var fs = require('fs');
var path = require('path');

var extractSpecTests = function(data) {
    var examples = [];
    var current_section = "";
    var example_number = 0;
    var tests = data
        .replace(/\r\n?/g, "\n") // Normalize newlines for platform independence
        .replace(/^<!-- END TESTS -->(.|[\n])*/m, '');

    tests.replace(/^`{32} example\n([\s\S]*?)^\.\n([\s\S]*?)^`{32}$|^#{1,6} *(.*)$/gm,
              function(_, markdownSubmatch, htmlSubmatch, sectionSubmatch){
                  if (sectionSubmatch) {
                      current_section = sectionSubmatch;
                  } else {
                      example_number++;
                      examples.push({markdown: markdownSubmatch,
                                     html: htmlSubmatch,
                                     section: current_section,
                                     number: example_number});
                  }
              });
    return examples;
};

var extractElmHtmlSpecTests = function(data) {
    var examples = [];
    var current_section = "";
    //var example_number = 0;
    var tests = data
        //.replace(/\r\n?/g, "\n") // Normalize newlines for platform independence
        //.replace(/^<!-- END TESTS -->(.|[\n])*/m, '');
        .replace(/^    [[|,] div \[ class "(\d+)" ]\n((?:[\s\S]*?)\n        ])/gm,
    //tests.replace(/^`{32} example\n([\s\S]*?)^\.\n([\s\S]*?)^`{32}$|^#{1,6} *(.*)$/gm,
              function(_, example_number, htmlSubmatch){
                  //example_number++;
                  examples[example_number] =
                    { html: htmlSubmatch,
                      number: example_number
                    };
              });
    return examples;};


var text = fs.readFileSync(path.join(__dirname, 'spec.txt'), 'utf8');
var tests = extractSpecTests(text);
var textElmHtml = fs.readFileSync(path.join(__dirname, 'TestsOk.elm'), 'utf8');
var elmHtml = extractElmHtmlSpecTests(textElmHtml);

var elmCode =
  "module Spec exposing (..)\n\n"
  + "import Html exposing (..)\n"
  + "import Test.Helpers exposing (..)\n\n\n"
  + "run : List (Output msg)\n"
  + "run =\n";
var testToElm = function(currentValue, index, array) {
    elmCode +=
      "    " + 
      (index==0?"[ ":", ") +
      "testEq " + currentValue.number + "\n"
      + "        []\n"
      + "        " + JSON.stringify(currentValue.markdown) + "\n"
      + (elmHtml[currentValue.number]?elmHtml[currentValue.number].html:"        []")
      //+ "\n        ]"
      + "\n\n";
};
tests.forEach(testToElm);
elmCode += "    ]\n"

var htmlToElm = function() {
  var code = "<article>";

  var htmlOutput = function(currentValue, index, array) {
      if (currentValue.section == "HTML blocks"
           || currentValue.number == 296
           || currentValue.number == 306
           || currentValue.number == 323
           || currentValue.number == 450
           || currentValue.number == 451
           || currentValue.number == 463 //462?
           || currentValue.number == 492
           || currentValue.number == 504
           || currentValue.number == 505
           || currentValue.number == 581
           || currentValue.number == 582
           || currentValue.number == 583
           || currentValue.number == 584
           || currentValue.number == 591 //592
           || currentValue.number == 596
           || currentValue.number == 597
           || currentValue.number == 598
           || currentValue.number == 599 //600
           || currentValue.number == 600 //601
           || currentValue.number == 611 //612
           || currentValue.number == 612 //613
         ) {

      } else {

        code +=
          "<div class=\""+currentValue.number+"\">\n"
          + currentValue.html
            //.replace(/&quot;/gm, '"')
            //.replace(/\\/gm, "\\\\")
          + "</div>\n\n"
          ;
      }
  };
  tests.forEach(htmlOutput);
  code += "</article>";
  return code;
};

fs.writeFile(path.join(__dirname, 'SpecTests.elm'), elmCode, function(error) {
     if (error) {
       console.error("write error: " + error.message);
     } else {
       console.log("Successful Write to " + path);
     }
});