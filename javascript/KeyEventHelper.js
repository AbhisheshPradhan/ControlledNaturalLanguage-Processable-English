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
          } else if( textLineData.nodes[textLineData.nodes.length - 1] == " " ) {
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

    console.log(preText);
    
    //What to do when we find a punctuation?
    //Is the punctuation also added to the nodes? i.e. posted to server as token??
    if(hasPunctuation) {
      //if it has punctuation, return proper array with punctuations splitted
      preText = preText.split(" ");
      if(preText[preText.length - 1] == "") {
        preText.pop();
      }

      preText.forEach((item) => {
        if(item.indexOf(".") != -1) {
          words.push(item.substring(0, item.length - 1));
          words.push(".");

          console.log(item.substring(0, item.length - 1));
        } else {
          words.push(item);
        }
      })
      console.log(words);
      return words;
    } else if(preText.indexOf(" ") > 0) {
      preText = preText.split(" ");
      // preText.pop(); //remove the last item which is ""
      if(preText[preText.length - 1] == "") {
        preText.pop();
      }
      // console.log("preText : ", preText);
      words = words.concat(preText);
      return words;
    } else {
      return words;
    }
  },

  //NEED TO POST EVERY ITEM THAT IS MISSING IN textLineData.nodes from words.
  //if textLineData.nodes.length < words.length
  //otherwise 
  alertPrevWord() {
    var text = document.getElementById("text_field");
    var caretPos = KeyHandler.getCaretPosition(text);
    var words = KeyHandler.returnWord(text.value, caretPos);

    if(words) {
      if(words.length - textLineData.nodes.length >= 2) {
        // console.log("words", words);
        // console.log("words not in textLineData ", words);
        for(let i = textLineData.nodes.length; i < words.length; i++) {
          viewModel.postToken(words[i]);
        }
        textLineData.nodes = words;
        // viewModel.postToken(words.pop());
        viewModel.lookUpTable(viewModel.lookahead.wordTable);
        viewModel.currentInitialLookUpTable = viewModel.lookahead.wordTable;
      } else {
        
        textLineData.nodes = words;
        // console.log("popped word : ",  words.pop())
        viewModel.postToken(words.pop());
        viewModel.lookUpTable(viewModel.lookahead.wordTable);
        viewModel.currentInitialLookUpTable = viewModel.lookahead.wordTable;
      }
    }
    

    //Re Init lookup table when we find .
    if(words[words.length - 1] == ".") {
      words.push(" ");
      textLineData.nodes = words;
      // console.log("popped word : ",  words.pop())
      viewModel.postToken(words.pop());
      viewModel.lookUpTable(viewModel.lookahead.wordTable);
      viewModel.currentInitialLookUpTable = viewModel.lookahead.wordTable;
    }
    console.log("words", words);
  }
}
