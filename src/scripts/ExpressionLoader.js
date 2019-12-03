let expressionLoader = {

  loadLookahead: function () {
    let lookaheadTable = lookaheadObj.createLookaheadTable(lookaheadObj);
    viewModel.lookaheadObject(lookaheadTable);
  },

  updateLookUpTable: function () {
    let tempLookAheadTable = [].concat(viewModel.lookUpTable());
    viewModel.lookUpTable(lookaheadObj.filterTable(viewModel.token(), tempLookAheadTable));
  },

  populateLookUpTable: function (data) {
    lookaheadObj.setAll(data);
    viewModel.lookUpTable(lookaheadObj.getWordTable());
  },

  postLookaheadWord: function (data, event) {
    viewModel.prevInputFromDropdown = true;
    viewModel.isDropdownInput = true;
    if (typeof data.add != "undefined") {
      if (confirm('Are you sure you want to add "' + data.add + '" to the temporary vocabulary?')) {
        lookaheadObj.createLookaheadTable(lookaheadObj);
        let catIndex = 0;
        let cat = $(event.target).parent().parent().parent().siblings().html();
        for (i = 0; i < lookaheadObj.cats.length; i++) {
          if (cat == lookaheadObj.cats[i]) {
            catIndex = i;
            break;
          }
        }
        let num = lookaheadObj.nums[catIndex];
        let vform = (num == " ") ? "bse" : "fin";
        let wform = data.add;
        this.addToLexicon(cat, wform, vform, num);
        textLineData.removeTailNode();
        // console.log("nodes", textLineData.nodes);
        let dataArr1 = wform.split(" ");
        let str = viewModel.textAreaStr().toString();
        str = str.replace(/^\s+|\s+$/g, "");
        let pos = str.lastIndexOf(" ");
        str = str.substring(0, pos + 1);
        viewModel.textAreaStr(str);

        // When puntuation mark is entered after a new content word
        if (viewModel.token() == "." || viewModel.token() == "?" || viewModel.token() == ",") {
          for (let s1 = 0; s1 < dataArr1.length; s1++) {
            viewModel.isDropdownInput = true;
            viewModel.updateViewForWord(dataArr1[s1]);
            viewModel.updateViewForWord(viewModel.token());
            viewModel.textAreaStr(viewModel.textAreaStr() + dataArr1[s1] + viewModel.token());
            viewModel.token(" ");
            this.loadLookahead();
            viewModel.$text_field.val(viewModel.textAreaStr());
          }
        } else {
          for (let s1 = 0; s1 < dataArr1.length; s1++) {
            viewModel.isDropdownInput = true;
            // viewModel.updateViewForWord(viewModel.token() + dataArr1[s1]);
            viewModel.updateViewForWord(dataArr1[s1]);
            viewModel.textAreaStr(viewModel.textAreaStr() + dataArr1[s1] + " ");
            this.loadLookahead();
            viewModel.$text_field.val(viewModel.textAreaStr());
          }
        }
      } else {
        return false;
      }
    } else if (viewModel.allowInput) {
      let dataArr = data.split(" ");
      // console.log("dataArr", dataArr);
      for (let s = 0; s < dataArr.length; s++) {
        viewModel.isDropdownInput = true;
        // viewModel.updateViewForWord(viewModel.token() + dataArr[s]);
        viewModel.updateViewForWord(dataArr[s]);
        // viewModel.token(dataArr[s]);
        console.log("viewModel.token()", viewModel.token());
        if (dataArr[s] == "." || dataArr[s] == "?") {
          viewModel.isEndOfSentence = true;
          // The text editor sentence will have space in the end
          viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1) + dataArr[s] + " ");
          viewModel.$text_field.val(viewModel.textAreaStr());

          viewModel.lookaheadObject(viewModel.initLookUpObj);
          viewModel.lookUpTable(viewModel.initLookUpTable);
        } else if (data == ",") {
          viewModel.isEndOfSentence = false;
          viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1) + dataArr[s] + " ");
          viewModel.$text_field.val(viewModel.textAreaStr());
        } else {
          viewModel.isEndOfSentence = false;

          
          // When user enter "." manually without space, add space after "." when loading word with dropdown
          if(textLineData.nodes[textLineData.nodes.length - 2] == " " && viewModel.textAreaStr()[viewModel.textAreaStr().length - 1] != " ")  {
            viewModel.textAreaStr(viewModel.textAreaStr() + " " + dataArr[s] + " ");
          } else {
            viewModel.textAreaStr(viewModel.textAreaStr() + dataArr[s] + " "); 
          }

          viewModel.$text_field.val(viewModel.textAreaStr());
          this.loadLookahead();
        }
      }
    }
  },

  postAnaExpClicked: function (words) {
    viewModel.isDropdownInput = true;
    let wordArr = "" + words;
    let word = wordArr.split(" ");
    if (viewModel.lookUpTable().indexOf(word[0]) != -1) {
      if (word.length > 1) {
        for (k = 0; k < word.length; k++) {
          viewModel.updateViewForWord(word[k]);
          viewModel.textAreaStr(viewModel.textAreaStr() + word[k] + " ");
          viewModel.$text_field.val(viewModel.textAreaStr());
        }
      } else {
        viewModel.updateViewForWord(word[0]);
        viewModel.textAreaStr(viewModel.textAreaStr() + word[0] + " ");
        viewModel.$text_field.val(viewModel.textAreaStr());
      }
      this.loadLookahead();
    } else {
      alert("The anaphoric expression \"" + word + "\" is not a valid token and will not be submitted. " + "Please try another input.");
    }
  },

  addToLexicon: function (cat, wform, vform, num) {
    let idNum = textLineData.nodes.length;
    let fs = this.createFS(cat, wform, vform, num);
    let addData = {
      "id": idNum,
      "inputmode": "text",
      "editmode": "add",
      "token": wform,
      "featurestructure": fs,
      "filename": " ",
      "spectext": " ",
      "snum": textLineData.sentences.length + 1,
      "spos": textLineData.sposNum - 1,
      "reasoner": "off",
      "reasonermode": "normal"
    }

    $.ajax({
      url: "/peng",
      type: "POST",
      data: addData
    });
  },

  // HELPER functions
  createFS: function (cat, wform, vform, num) {
    let fs = new Object()
    if (cat == "verb: intransitive" || cat == "verb: transitive") {
      fs.cat = cat;
      fs.wform = wform;
      fs.vform = vform;
      fs.num = num;
    }
    else if (cat == "name") {
      fs.cat = cat;
      fs.wform = wform;
      fs.num = num;
    }
    else if (cat == "noun: common" || cat == "noun: relational") {
      fs.cat = cat;
      fs.wform = wform;
      fs.num = num;
    }
    else if (cat == "adjective" || cat == "adjective: relational") {
      fs.cat = cat;
      fs.wform = wform;
    }
    return JSON.stringify(fs);
  }
}
