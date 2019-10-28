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
    viewModel.isDropdownInput = false;

    console.log("chr", chr);
    console.log("viewModel.token()", viewModel.token());

    // when backspace to . then pressing space, 
    if(chr == "" && viewModel.token() == " ") {
      console.log("no need to update view")
      return;
    }

    if ((viewModel.token() == "." || viewModel.token() == "?") && textLineData.nodes[textLineData.nodes.length - 1] != " ") {
      // reinit when space typed after . or ?
      viewModel.updateViewForWord(" ");
    } else {
      // update for typed word
      if (viewModel.token() != "." || viewModel.token() != "?") {
        viewModel.updateViewForWord(viewModel.token().slice(0, sizeOfWord));
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
        let chr = viewModel.token() + keyVal;
        viewModel.token(chr);
    }
  },


  // viewModel.textAreaStr() is ko var for the strings in text editor
  // viewModel.$text_field.val() is the var to get the value of HTML element
  // when backspace is pressed, chars are deleted from viewModel.$text_field.val()
  // so it will not equal to viewModel.$text_field.val()
  // 
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

        if(currentIndexBehindPreviousToken) {
          var tokenToBeDel = textLineData.nodes[textLineData.nodes.length - 1];
          console.log("tokenToBeDel", tokenToBeDel)

          if(tokenToBeDel == "." || tokenToBeDel == "?" || tokenToBeDel == "," ) {
            var prevNode = textLineData.removeTailNode();
            prevNode = textLineData.removeTailNode();
            viewModel.token(prevNode);
            prevNode = textLineData.removeTailNode();
            console.log("need to delete 3 tokens");
            console.log("prevNode", prevNode);
            if(tokenToBeDel == "." || tokenToBeDel == "?" ) {
              textLineData.removeSentence();
            }
            viewModel.isDropdownInput = false;
            viewModel.updateViewForWord(prevNode);
            viewModel.lookUpTable(lookaheadObj.wordTable);
            viewModel.currentInitialLookUpTable = lookaheadObj.wordTable;
            console.log("nodes", textLineData.nodes);
          } else if(tokenToBeDel != " "){
            var prevNode = textLineData.removeTailNode();
            viewModel.token(prevNode);
            prevNode = textLineData.removeTailNode();
            
            if(this.isLastWordOfSentence) {
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
  }
}
