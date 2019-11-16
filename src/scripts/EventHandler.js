// Modified by Rolf Schwitter: 1.2.2019

var eventHandler = {

  isLastWordOfSentence: false,

  keyUpdate: function (d, e) {
    var keyID = e.keyCode;
    var keyVal = (String.fromCharCode(keyID)); // Character form
    let enterKeyPressed = keyID == 13 ? true : false;

    if (enterKeyPressed) {
      this.enterKey();
      return false;
    }

    // first input should not be a space
    if (keyVal == " " && viewModel.textAreaStr().length == 0) {
      return false;
    }

    // no 2 spaces consecutively
    if (keyVal == " " && viewModel.textAreaStr().slice(-1) == " ") {
      return false;
    }

    if (viewModel.allowInput) {
      this.switchKeyVal(keyVal);
      keyVal = (keyID == 13) ? "" : keyVal;
      viewModel.textAreaStr(viewModel.textAreaStr() + keyVal);
      viewModel.updateLookUpTable();
      viewModel.prevInputFromDropdown = false;
    }
    return true;
  },

  // Submit
  enterKey: function () {
    console.log("isEndOfSentence", viewModel.isEndOfSentence);
    if (viewModel.isEndOfSentence || textLineData.nodes[textLineData.nodes.length - 1] == " ") {
      viewModel.textList.removeAll();
      for (let i = 0; i < textLineData.sentences.length; i++) {
        viewModel.textList.push(textLineData.sentences[i]);
      }
    }
  },

  punctuation: function (chr) {
    // Post the word before punctuation first
    var sizeOfWord = viewModel.token().length;

    // when backspace to . then pressing space, 
    if (chr == "" && viewModel.token() == " ") {
      return;
    }

    // when entering a space after . 
    if (chr == "" && viewModel.token() == "." && textLineData.nodes[textLineData.nodes.length - 1] == " ") {
      return;
    }

    if ((viewModel.token() == "." || viewModel.token() == "?") && textLineData.nodes[textLineData.nodes.length - 1] != " ") {
      // reinit when space typed after . or ?
      viewModel.updateViewForWord(" ");
    } else if (chr == "" && (viewModel.token() != "." || viewModel.token() != "?") && textLineData.nodes[textLineData.nodes.length - 1] != " ") {
      viewModel.updateViewForWord(viewModel.token());
    } else {
      // update for typed word
      if ((viewModel.token() != "." || viewModel.token() != "?") && !viewModel.prevInputFromDropdown) {
        if(viewModel.token() == "" && (chr == "." || chr == "?")) {
          viewModel.updateViewForWord(chr);
        } else {
          viewModel.updateViewForWord(viewModel.token().slice(0, sizeOfWord));
        }
      }

      // if chr is . or ?, update for it and set end of sentence
      if (chr == '.' || chr == '?') {
        if (viewModel.allowInput && $.inArray(chr, viewModel.lookUpTable()) != -1) {
          viewModel.updateViewForWord(chr);
          viewModel.isEndOfSentence = true;
        }
      }
    }
  },

  switchKeyVal: function (keyVal) {
    switch (keyVal) {
      case ',':
        eventHandler.punctuation('');
        viewModel.token(",");
        break;
      case ' ':
        eventHandler.punctuation('');
        viewModel.token('');
        break;
      case '.':
      case '?':
        eventHandler.punctuation(keyVal);
        viewModel.token(keyVal);
        break;
      case String.fromCharCode(13):  // Enter
        eventHandler.enterKey();
        break;
      default:
        console.log("viewModel.token() + keyVal", viewModel.token() + keyVal)
        viewModel.token(viewModel.token() + keyVal);
    }
  },

  // viewModel.textAreaStr() is ko var for the strings in text editor
  // viewModel.$text_field.val() is the var to get the value of HTML element
  // when backspace is pressed, chars are deleted from viewModel.$text_field.val()
  // so it will not equal to viewModel.$text_field.val()
  backspace: function () {
    //backspace detected
    if (viewModel.$text_field.val().length > 0) {
      while (viewModel.textAreaStr() != viewModel.$text_field.val()) {
        let charBeingRemoved = viewModel.textAreaStr().slice(viewModel.textAreaStr().length - 1, viewModel.textAreaStr().length);

        // deleting chars from currently constructing token when backspace pressed
        viewModel.token(viewModel.token().slice(0, viewModel.token().length - 1));

        // update ko variable textAreaStr
        viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1));

        let currentIndexBehindPreviousToken = charBeingRemoved == " " || charBeingRemoved == "," || charBeingRemoved == "." ||
          charBeingRemoved == "?";

        if (currentIndexBehindPreviousToken) {
          var tokenToBeDel = textLineData.nodes[textLineData.nodes.length - 1];
          console.log("tokenToBeDel", tokenToBeDel)

          if (tokenToBeDel == "." || tokenToBeDel == "?" || tokenToBeDel == ",") {
            var prevNode = textLineData.removeTailNode();
            prevNode = textLineData.removeTailNode();
            viewModel.token(prevNode);
            prevNode = textLineData.removeTailNode();
            console.log("need to delete 3 tokens");
            console.log("prevNode", prevNode);
            if (tokenToBeDel == "." || tokenToBeDel == "?") {
              textLineData.removeSentence();
            }
            viewModel.isDropdownInput = false;
            viewModel.updateViewForWord(prevNode);
            viewModel.lookUpTable(lookaheadObj.wordTable);
            viewModel.currentInitialLookUpTable = lookaheadObj.wordTable;
            console.log("nodes", textLineData.nodes);
          } else if (tokenToBeDel != " ") {
            var prevNode = textLineData.removeTailNode();
            viewModel.token(prevNode);
            prevNode = textLineData.removeTailNode();

            if (this.isLastWordOfSentence) {
              prevNode = textLineData.removeTailNode();
            }
            console.log("word deleted")
            console.log("prevNode", prevNode);

            viewModel.isDropdownInput = false;
            viewModel.updateViewForWord(prevNode);
            viewModel.lookUpTable(lookaheadObj.wordTable);
            viewModel.currentInitialLookUpTable = lookaheadObj.wordTable;
            console.log("nodes", textLineData.nodes);
            this.isLastWordOfSentence = false;
          } else {
            var prevNode = textLineData.removeTailNode();
            viewModel.token(prevNode);
            viewModel.isEndOfSentence = true;
            console.log("empty token deleted");

            this.isLastWordOfSentence = false;
          }
        }
      }
    } else {
      console.log("text area cleared");
      console.log("sentences", textLineData.sentences);
      console.log("nodes", textLineData.nodes);
      console.log("token", viewModel.token());
      console.log("viewModel.textAreaStr()", viewModel.textAreaStr())
      textLineData.nodes = [];
      textLineData.sentences = [];
      textLineData.sposNum = 0;
      viewModel.textAreaStr("")
      viewModel.token("");
      console.log("text area cleared");
      console.log("sentences", textLineData.sentences);
      console.log("nodes", textLineData.nodes);
      console.log("token", viewModel.token());
      console.log("viewModel.textAreaStr()", viewModel.textAreaStr())
      viewModel.init();
    }
  },

  getCaretPosition(ctrl) {
    var CaretPos = 0;   // IE Support
    if (document.selection) {
      ctrl.focus();
      var Sel = document.selection.createRange();
      Sel.moveStart('character', -ctrl.value.length);
      CaretPos = Sel.text.length;
    }
    // Firefox support
    else if (ctrl.selectionStart || ctrl.selectionStart == '0')
      CaretPos = ctrl.selectionStart;
    return (CaretPos);
  },

  //Functions to get the word
  returnWord(text, caretPos) {
    var index = text.indexOf(caretPos);
    var preText = text.substring(0, caretPos);
    var hasPunctuation = preText.indexOf(".") != -1 || preText.indexOf(",") != -1 || preText.indexOf("?") != -1;
    var words = [" "];

    //If the cursor is between letters of a word and the cursor is not between the last word and full-stop
    //eg. this is an example|.
    //we set preText to previous word otherwise we select the last word as well
    if (text[preText.length] != " " && preText[preText.length - 1] != "." && text[preText.length] != ".") {
      preText = text.substring(0, preText.lastIndexOf(" "));
    }

    if (hasPunctuation) {
      //if it has punctuation, return proper array with punctuations splitted
      preText = preText.split(" ");
      if (preText[preText.length - 1] == "") {
        preText.pop();
      }

      //preText is a string, so we need to separate . and ? when we find one then push it to word
      preText.forEach((item) => {
        if (item.indexOf(".") != -1) {
          words.push(item.substring(0, item.length - 1));
          words.push(".");
          // words.push(" ");
        } else if (item.indexOf("?") != -1) {
          words.push(item.substring(0, item.length - 1));
          words.push("?");
          // words.push(" ");
        } else if (item.indexOf(",") != -1) {
          words.push(item.substring(0, item.length - 1));
          words.push(",");
        } else {
          words.push(item);
        }
      })
      return words;
    } else {
      preText = preText.split(" ");
      //remove the last item which is ""
      if (preText[preText.length - 1] == "") {
        preText.pop();
      }
      words = words.concat(preText);
      return words;
    }
  },

  //NEED TO POST EVERY ITEM THAT IS MISSING IN textLineData.nodes from words.
  //if textLineData.nodes.length < words.length
  //otherwise 
  //Trying to replace the word instead of posting at the end of the sentence
  //maybe try making text, words, wordsintextbox a property of KeyHandler so it can be accessed from viewModel.
  alertPrevWord() {
    let textField = document.getElementById("text_field");
    let caretPos = this.getCaretPosition(textField);
    let words = this.returnWord(textField.value, caretPos);

    console.log("caretPos", caretPos);
    console.log("words", words);

    console.log("wordsInTextbox", textLineData.nodes)


    if (words) {
      textLineData.nodes = [];
      textLineData.sentences = [];
      textLineData.sposNum = 0;
      viewModel.textAreaStr("")
      viewModel.token("");
      viewModel.isCursorInput = true;

      words.forEach((word) => {
        viewModel.updateViewForWord(word);
      });
      
      viewModel.isCursorInput = false;
    }
  },
}
