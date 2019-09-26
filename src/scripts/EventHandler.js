// Modified by Rolf Schwitter: 1.2.2019

var eventHandler = {
  keyUpdate: function (d, e) {
    var keyID = e.keyCode;
    var keyVal = (String.fromCharCode(keyID)); // Character form
    let enterKeyPressed = keyID == 13 ? true : false;

    if (enterKeyPressed) {
      console.log("enter pressed");
      this.enterKey();
    }

    // var charAllowed = (keyID != 13 && viewModel.token().charAt(viewModel.token().length - 1) != "." && viewModel.token().charAt(viewModel.token().length - 1) != "?") || keyID == 13;

    // if (keyVal == " " && viewModel.textAreaStr().length == 0) {
    //   return false;
    // }

    // if (keyVal == " " && viewModel.textAreaStr().slice(-1) == " ") {
    //   return false;
    // }

    // if (!charAllowed) {
    //   return false;
    // }

    // if (viewModel.allowInput) {
    //   this.switchKeyVal(keyVal);
    //   keyVal = (keyID == 13) ? "" : keyVal;
    //   viewModel.textAreaStr(viewModel.textAreaStr() + keyVal);
    //   viewModel.updateLookUpTable();
    // }
    return true;
  },

  // Submit action happens here
  // Need to create and array and split up sentences and add them as sentences..push them into textList 
  //and then clear everything. Or not??
  enterKey: function () {
    console.log("sentence submitted")
    var isEndOfSentence = viewModel.isEndOfSentence;
    if (isEndOfSentence) {
      viewModel.textList.removeAll();
      for (let i = 0; i < textLineData.sentences.length; i++) {
        viewModel.textList.push(textLineData.sentences[i]);
        // viewModel.textAreaStr('');
        // viewModel.$text_field.val('');
        // viewModel.init();
      }
    }

    // console.log("isEndOfSentence : ", isEndOfSentence);
  },

  punctuation: function (chr) { //str is what to update currentWord
    var sizeOfWord = viewModel.token().length;
    viewModel.updateViewForWord(viewModel.token().slice(0, sizeOfWord));
    viewModel.firstIndexOfCurrentWord = viewModel.textAreaStr().length + 1;
    if (chr == '.' || chr == '?') {
      if (viewModel.allowInput && $.inArray(chr, viewModel.lookUpTable()) != -1) {
        viewModel.updateViewForWord(chr);
      } else {
        textLineData.sposNum--;
        // viewModel.allowInput = false;
        console.log("allowInput false");
      }
    }
  },

  backspace: function (d, e) {
    var counter = 0;
    while (viewModel.textAreaStr() != viewModel.$text_field.val()) {
      viewModel.token(viewModel.token().slice(0, viewModel.token().length - 1));
      var charBeingRemoved = viewModel.textAreaStr().slice(viewModel.textAreaStr().length - 1, viewModel.textAreaStr().length);
      viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1));
      // When backspace all the way to previous token
      var currentIndexInPreviousToken = charBeingRemoved == " " || charBeingRemoved == "," ||
        charBeingRemoved == "." || charBeingRemoved == "?";
      if (currentIndexInPreviousToken) {
        var tokenToBeDel = textLineData.nodes[textLineData.nodes.length - 2];
        this.updateToPreviousToken(charBeingRemoved);
        //MIGHT NEED CHANGES
        if (tokenToBeDel != " ") {
          var pop = textLineData.removeTailNode();
          viewModel.updateViewForWord(pop);
          viewModel.lookUpTable(lookaheadObj.wordTable);
          viewModel.currentInitialLookUpTable = lookaheadObj.wordTable;
        } else if (textLineData.nodes[textLineData.nodes.length - 1] == " ") {
          viewModel.currentInitialLookUpTable = viewModel.initLookUpTable;
          viewModel.lookaheadObject(viewModel.initLookUpObj);
          viewModel.lookUpTable(viewModel.initLookUpTable);
        }
        viewModel.allowInput = true;
      } else {
        viewModel.lookUpTable(viewModel.currentInitialLookUpTable);
      }
      counter++;
      if (counter > 50) {
        viewModel.textAreaStr(viewModel.$text_field.val());
        break;
      }
    }
    viewModel.allowInput = true;
  },

  updateToPreviousToken(charBeingRem) {
    var prevToken = textLineData.removeTailNode(); //also removes prev token
    if (prevToken == '.' || prevToken == '?') {
      prevToken = textLineData.removeTailNode();
    }
    viewModel.token(prevToken);
    viewModel.firstIndexOfCurrentWord = viewModel.firstIndexOfCurrentWord - viewModel.token().length - 1;
    if (charBeingRem == ",")
      viewModel.firstIndexOfCurrentWord++;
  },

  switchKeyVal: function (keyVal) {
    switch (keyVal) {
      case ',':
        eventHandler.punctuation('');
        viewModel.token(",");
        break;
      case ' ': //SPACE CLICKED
        eventHandler.punctuation('');
        viewModel.token('');
        break;
      case '.':
      case '?':
        eventHandler.punctuation(keyVal);
        viewModel.token(viewModel.token() + keyVal);
        break;
      case String.fromCharCode(13):  // Enter
        eventHandler.enterKey();
        break;
      default:
        viewModel.token(viewModel.token() + keyVal);
    }
  }
}
