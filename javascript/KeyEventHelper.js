// Modified by Rolf Schwitter: 1.2.2019

var KeyHandler = {

  
  keyUpdate: function(d, e) {
    var keyID = e.keyCode;
    var keyVal = (String.fromCharCode(keyID)); // Character form
    var charAllowed = (keyID != 13 && viewModel.token().charAt(viewModel.token().length-1) != "." && viewModel.token().charAt(viewModel.token().length-1) != "?") || keyID == 13;
 
    if(keyVal == " " && viewModel.textAreaStr().length == 0) {
      return false; }

    if(keyVal == " " && viewModel.textAreaStr().slice(-1) == " ") {
      return false; }

    if(!charAllowed)
	return false;
      
    if(viewModel.allowInput) {
      this.switchKeyVal(keyVal);
      keyVal = (keyID == 13) ? "" : keyVal;
      viewModel.textAreaStr(viewModel.textAreaStr()+keyVal);
      viewModel.updateLookUpTable();
    }
    return true;
  },

  enterKey: function() {
    var isEndOfSentence = viewModel.token().charAt(viewModel.token().length-1) == "." || viewModel.token().charAt(viewModel.token().length-1) == "?";
    if(isEndOfSentence) {
      textLineData.addSentence(viewModel.textAreaStr());
      viewModel.textList.push(viewModel.textAreaStr());
      viewModel.textAreaStr('');
      viewModel.token('');
      viewModel.$text_field.val('');
      viewModel.init();
    }
  },

  punctuation: function(chr) { //str is what to update currentWord
    var sizeOfWord = viewModel.token().length;
    viewModel.postToken(viewModel.token().slice(0, sizeOfWord));

    viewModel.firstIndexOfCurrentWord = viewModel.textAreaStr().length + 1;
    if (chr == '.' || chr == '?') {
      if(viewModel.allowInput && $.inArray(chr, viewModel.lookUpTable()) != -1)
        viewModel.postToken(chr);
      else {
        textLineData.sposNum--;
        viewModel.allowInput = false;
      }
    }
  },

    
  backspace: function(d, e) {
    var keyVal = e.keyCode;

    if (keyVal == 8) { //backspace detected
      var counter = 0;
      while (viewModel.textAreaStr() != viewModel.$text_field.val()) {
        viewModel.asyncFlag = false; // should be true
        viewModel.token(viewModel.token().slice(0, viewModel.token().length-1));
        var charBeingRemoved = viewModel.textAreaStr().slice(viewModel.textAreaStr().length-1, viewModel.textAreaStr().length);
        viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length-1));
        // When backspace all the way to previous token

        //var currentIndexInPreviousToken = viewModel.textAreaStr().length == viewModel.firstIndexOfCurrentWord-1;
          var currentIndexInPreviousToken = charBeingRemoved == " " || charBeingRemoved == "," ||
	      charBeingRemoved == "." || charBeingRemoved == "?";
        if (currentIndexInPreviousToken) {
          var tokenToBeDel = textLineData.nodes[textLineData.nodes.length-2];
          this.updateToPreviousToken(charBeingRemoved);
          //MIGHT NEED CHANGES
          if(tokenToBeDel != " ") {
            var pop = textLineData.removeTailNode();
            viewModel.postToken(pop);
            viewModel.lookUpTable(viewModel.lookahead.wordTable);
            viewModel.currentInitialLookUpTable = viewModel.lookahead.wordTable;
          }
            else if( textLineData.nodes[textLineData.nodes.length-1] == " " ){
            viewModel.currentInitialLookUpTable = viewModel.initSentenceLookUp;
            viewModel.lookaheadObject(viewModel.initLookUpObj);
            viewModel.lookUpTable(viewModel.initSentenceLookUp);
          }

          viewModel.allowInput = true;
        }
        else {
          viewModel.lookUpTable(viewModel.currentInitialLookUpTable);
        }
        counter++;

        if(counter > 50) {
          viewModel.textAreaStr(viewModel.$text_field.val());
          break;
        }
      }
      viewModel.allowInput = true;
      viewModel.asyncFlag = false;
    }
  },

  updateToPreviousToken(charBeingRem) {
    var prevToken = textLineData.removeTailNode(); //also removes prev token
    if (prevToken == '.' || prevToken == '?')
      prevToken = textLineData.removeTailNode();
    viewModel.token(prevToken);
    viewModel.firstIndexOfCurrentWord = viewModel.firstIndexOfCurrentWord-viewModel.token().length -1;
    if(charBeingRem == ",")
      viewModel.firstIndexOfCurrentWord++;
  },

  switchKeyVal: function(keyVal) {
    switch(keyVal) {
      case ',':
        KeyHandler.punctuation('');
        viewModel.token(",");
        break;
      case ' ': //SPACE CLICKED
        KeyHandler.punctuation('');
        viewModel.token('');
        break;
      case '.':
      case '?':
        KeyHandler.punctuation(keyVal);
        viewModel.token(viewModel.token()+keyVal);
        break;
      case String.fromCharCode(13):  // Enter
        KeyHandler.enterKey();
        break;
      default:
        viewModel.token(viewModel.token()+keyVal);
    }
  },



}
