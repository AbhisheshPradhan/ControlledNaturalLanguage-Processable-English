let eventHandler = {

  keyUpdate: function (d, e) {
    let keyID = e.keyCode;
    let keyVal = (String.fromCharCode(keyID)); // Character form
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
    let sizeOfWord = viewModel.token().length;
    viewModel.isDropdownInput = false;

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

      // For space + punctuation, remove the space before punctuation
      let currentTextFieldStr = viewModel.$text_field.val();
      if( currentTextFieldStr[currentTextFieldStr.length - 2] == " ") {
        viewModel.$text_field.val(viewModel.$text_field.val().slice(0, viewModel.$text_field.val().length - 2) + viewModel.token());
        viewModel.textAreaStr(viewModel.$text_field.val());
      }

      // console.log()
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
          // console.log("textAreaStr", viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1) + chr);
          // console.log();
          // viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1) + chr);
          // viewModel.$text_field.val(viewModel.textAreaStr());


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
        console.log("viewModel.token() + keyVal", viewModel.token() + keyVal);
        viewModel.token(viewModel.token() + keyVal);
    }
  },

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
          let tokenToBeDel = textLineData.nodes[textLineData.nodes.length - 1];
          console.log("tokenToBeDel", tokenToBeDel)

          if (tokenToBeDel == "." || tokenToBeDel == "?" || tokenToBeDel == ",") {
            let prevNode = textLineData.removeTailNode();
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
            let prevNode = textLineData.removeTailNode();
            viewModel.token(prevNode);
            prevNode = textLineData.removeTailNode();

            console.log("word deleted")
            console.log("prevNode", prevNode);

            viewModel.isDropdownInput = false;
            viewModel.updateViewForWord(prevNode);
            viewModel.lookUpTable(lookaheadObj.wordTable);
            viewModel.currentInitialLookUpTable = lookaheadObj.wordTable;
            console.log("nodes", textLineData.nodes);
          } else {
            let prevNode = textLineData.removeTailNode();
            viewModel.token(prevNode);
            viewModel.isEndOfSentence = true;
            console.log("empty token deleted");

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

  _getCaretPosition: function(ctrl) {
    let CaretPos = 0;   // IE Support
    if (document.selection) {
      ctrl.focus();
      let Sel = document.selection.createRange();
      Sel.moveStart('character', -ctrl.value.length);
      CaretPos = Sel.text.length;
    }
    // Firefox support
    else if (ctrl.selectionStart || ctrl.selectionStart == '0')
      CaretPos = ctrl.selectionStart;
    return (CaretPos);
  },

  _returnWord: function (text, caretPos) {
    let preText = text.substring(0, caretPos);
    let hasPunctuation = preText.indexOf(".") != -1 || preText.indexOf(",") != -1 || preText.indexOf("?") != -1;
    let words = [" "];

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
        } else if (item.indexOf("?") != -1) {
          words.push(item.substring(0, item.length - 1));
          words.push("?");
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
  
  cursorLookaheadLoading: function () {
    let textField = document.getElementById("text_field");
    let caretPos = this._getCaretPosition(textField);
    let words = this._returnWord(textField.value, caretPos);

    // console.log("caretPos", caretPos);
    // console.log("words", words);
    // console.log("wordsInTextbox", textLineData.nodes);

    if (words) {
      textLineData.nodes = [];
      textLineData.sentences = [];
      textLineData.sposNum = 0;
      textLineData.snum = 0;
      viewModel.textAreaStr("")
      viewModel.token("");
      viewModel.isCursorInput = true;

      words.forEach((word) => {
        viewModel.updateViewForWord(word);
      });
    }
    viewModel.isCursorInput = false;
  },
}
