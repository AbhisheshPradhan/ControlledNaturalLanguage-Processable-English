// Modified by Rolf Schwitter: 1.2.2019

var eventHandler = {
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
    if (viewModel.isEndOfSentence) {
      viewModel.textList.removeAll();
      for (let i = 0; i < textLineData.sentences.length; i++) {
        viewModel.textList.push(textLineData.sentences[i]);
      }
    }
  },

  punctuation: function (chr) {
    console.log("punctuation detected", chr);

    // Post the word before punctuation first
    var sizeOfWord = viewModel.token().length;
    viewModel.updateViewForWord(viewModel.token().slice(0, sizeOfWord));

    // viewModel.firstIndexOfCurrentWord = viewModel.textAreaStr().length + 1;

    if (chr == '.' || chr == '?') {
      // If the sentence can be ended with . or ?
      if (viewModel.allowInput && $.inArray(chr, viewModel.lookUpTable()) != -1) {
        viewModel.updateViewForWord(chr);
        viewModel.isEndOfSentence = true;

        // reinit lookahead
        viewModel.lookaheadObject(viewModel.initLookUpObj);
        console.log("viewModel.initLookUpObj", viewModel.initLookUpObj);
        viewModel.lookUpTable(viewModel.initLookUpTable);
        console.log("viewModel.initLookUpTable", viewModel.initLookUpTable);

      } else {
        // The sentence cannot be ended with . or ?

        viewModel.isEndOfSentence = false;
        textLineData.sposNum--;
        // viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1));
        // viewModel.$text_field.val(viewModel.textAreaStr());
      }
    }
  },

  backspace: function (d, e) {
    alert("Backspace in progress!");
    // var counter = 0;
    // while (viewModel.textAreaStr() != viewModel.$text_field.val()) {
    //   viewModel.token(viewModel.token().slice(0, viewModel.token().length - 1));
    //   var charBeingRemoved = viewModel.textAreaStr().slice(viewModel.textAreaStr().length - 1, viewModel.textAreaStr().length);
    //   viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1));
    //   // When backspace all the way to previous token
    //   var currentIndexInPreviousToken = charBeingRemoved == " " || charBeingRemoved == "," ||
    //     charBeingRemoved == "." || charBeingRemoved == "?";
    //   if (currentIndexInPreviousToken) {
    //     var tokenToBeDel = textLineData.nodes[textLineData.nodes.length - 2];
    //     this.updateToPreviousToken(charBeingRemoved);
    //     //MIGHT NEED CHANGES
    //     if (tokenToBeDel != " ") {
    //       var pop = textLineData.removeTailNode();
    //       viewModel.updateViewForWord(pop);
    //       viewModel.lookUpTable(lookaheadObj.wordTable);
    //       viewModel.currentInitialLookUpTable = lookaheadObj.wordTable;
    //     } else if (textLineData.nodes[textLineData.nodes.length - 1] == " ") {
    //       viewModel.currentInitialLookUpTable = viewModel.initLookUpTable;
    //       viewModel.lookaheadObject(viewModel.initLookUpObj);
    //       viewModel.lookUpTable(viewModel.initLookUpTable);
    //     }
    //     viewModel.allowInput = true;
    //   } else {
    //     viewModel.lookUpTable(viewModel.currentInitialLookUpTable);
    //   }
    //   counter++;
    //   if (counter > 50) {
    //     viewModel.textAreaStr(viewModel.$text_field.val());
    //     break;
    //   }
    // }
    // viewModel.allowInput = true;
  },

  // updateToPreviousToken(charBeingRem) {
  //   var prevToken = textLineData.removeTailNode(); //also removes prev token
  //   if (prevToken == '.' || prevToken == '?') {
  //     prevToken = textLineData.removeTailNode();
  //   }
  //   viewModel.token(prevToken);
  //   viewModel.firstIndexOfCurrentWord = viewModel.firstIndexOfCurrentWord - viewModel.token().length - 1;
  //   if (charBeingRem == ",")
  //     viewModel.firstIndexOfCurrentWord++;
  // },

  switchKeyVal: function (keyVal) {
    console.log("keyVal : ", keyVal);
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
        viewModel.token(viewModel.token() + keyVal);
    }
  }
}
