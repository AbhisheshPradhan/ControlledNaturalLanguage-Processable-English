// Modified by Rolf Schwitter: 1.2.2019

var eventHandler = {

  lastInputEntered: null,

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
    }
    return true;
  },

  // Submit action happens here
  // Need to create and array and split up sentences and add them as sentences..push them into textList 
  //and then clear everything. Or not??
  enterKey: function () {
    // console.log("isEndOfSentence", viewModel.isEndOfSentence)
    if (viewModel.isEndOfSentence) {
      viewModel.textList.removeAll();
      for (let i = 0; i < textLineData.sentences.length; i++) {
        viewModel.textList.push(textLineData.sentences[i]);
      }
    }
  },

  punctuation: function (chr) {
    // Post the word before punctuation first
    var sizeOfWord = viewModel.token().length;
    eventHandler.lastInputEntered = chr;

    if (viewModel.token() != '.' && viewModel.token() != '?') {
      viewModel.updateViewForWord(viewModel.token().slice(0, sizeOfWord));

    } else if (viewModel.token() == '.' || viewModel.token() == '?') {
      if (viewModel.allowInput && $.inArray(viewModel.token(), viewModel.lookUpTable()) != -1) {
        viewModel.updateViewForWord(viewModel.token());
        viewModel.lookaheadObject(viewModel.initLookUpObj);
        viewModel.lookUpTable(viewModel.initLookUpTable);
      }
    }

    viewModel.firstIndexOfCurrentWord = viewModel.textAreaStr().length + 1;
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
        let chr = viewModel.token() + keyVal;
        viewModel.token(chr);
    }
  },


  backspace: function () {
    //backspace detected
    if (viewModel.$text_field.val().length > 0) {
      while (viewModel.textAreaStr() != viewModel.$text_field.val()) {
        // console.log("viewModel.textAreaStr()", viewModel.textAreaStr());
        // console.log("viewModel.$text_field.val()", viewModel.$text_field.val());

        let charBeingRemoved = viewModel.textAreaStr().slice(viewModel.textAreaStr().length - 1, viewModel.textAreaStr().length);
        let removeToken = false;
        viewModel.token(viewModel.token().slice(0, viewModel.token().length - 1));
        viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1));

        let charBeforeCharBeingRemoved = viewModel.textAreaStr().slice(viewModel.textAreaStr().length - 1, viewModel.textAreaStr().length);
        
        if (charBeingRemoved == "." || charBeingRemoved == "?") {
          // console.log("nodes before", textLineData.nodes);
          tokenToBeRemoved = textLineData.removeTailNode();
          // console.log("nodes after", textLineData.nodes);
          textLineData.removeSentence();
          removeToken = true;
        } else if (charBeforeCharBeingRemoved == " ") {
          // console.log("nodes before", textLineData.nodes);
          tokenToBeRemoved = textLineData.removeTailNode();
          // console.log("nodes after", textLineData.nodes);
          removeToken = true;
        } else if (charBeforeCharBeingRemoved == "." || charBeforeCharBeingRemoved == "?") {
          // console.log("nodes before", textLineData.nodes);
          tokenToBeRemoved = textLineData.removeTailNode();
          // console.log("nodes after", textLineData.nodes);
        }

        if (textLineData.lastSentenceNodes().length == 1) {
          viewModel.lookaheadObject(viewModel.initLookUpObj);
          viewModel.lookUpTable(viewModel.initLookUpTable);
        } else if (removeToken) {
          let prevToken = textLineData.removeTailNode();
          viewModel.updateViewForWord(prevToken);
          viewModel.lookUpTable(lookaheadObj.wordTable);
          viewModel.currentInitialLookUpTable = lookaheadObj.wordTable;
          // console.log("tokenToBeRemoved", tokenToBeRemoved)
          removeToken = false;
        }
      }
    } else if (viewModel.$text_field.val().length == 0) {
      // console.log("this._clearTextArea();")
      this._clearTextArea();
    }
  },

  _clearTextArea() {
    // console.log("text area empty");
    textLineData.nodes = [" "];
    textLineData.sentences = [];
    viewModel.textAreaStr("");
    viewModel.token("");
    viewModel.processedInput("");

    // console.log("viewModel.textAreaStr()", viewModel.textAreaStr());
    // console.log("viewModel.token()", viewModel.token())

    viewModel.lookaheadObject(viewModel.initLookUpObj);
    viewModel.lookUpTable(viewModel.initLookUpTable);
  }
}
