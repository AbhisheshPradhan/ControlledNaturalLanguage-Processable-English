/**
 *   Author: Rudz Evan Bernardez, Macquarie University
 *   File Name: ViewModel.js
 *
 */

let viewModel = {
  $text_field: $("#text_field"),
  $saveFileName: $("#saveFileName"),
  $loading: $("#loader-overlay"),

  textAreaStr: ko.observable(""),
  processedInput: ko.observable(""),
  token: ko.observable(""),
  textList: ko.observableArray([]),
  fileNames: ko.observableArray([]),

  result: ko.observable(""),
  smallAsp: "",
  bigAsp: "",
  aspState: false,
  asp: ko.observable(''),
  answer: ko.observable(''),

  initialLoad: true,
  allowInput: true,
  isEndOfSentence: false,

  catTable: ko.observableArray([]),
  anaExp: ko.observableArray([]),
  lookaheadObject: ko.observableArray([]),
  lookUpTable: ko.observableArray([]),
  initLookUpObj: [],
  initLookUpTable: [],
  currentInitialLookUpTable: [], //used in backspace handler

  isDropdownInput: false,
  isLoadFileInput: false,
  isCursorInput: false,
  prevInputFromDropdown: false,

  init: function () {
    console.log("init should be only called once or after sentence completion")
    let lastNodePostedWasBlank = textLineData.nodes[textLineData.nodes.length - 1] != " ";
    let textAreaEmpty = this.textAreaStr().length == 0;
    if (textAreaEmpty && lastNodePostedWasBlank) {
      this.updateViewForWord(" ");
      this.initLookUpTable = this.lookUpTable();
      this.initLookUpObj = this.lookaheadObject();
    }
  },

  loadLookahead: function () {
    if (this.initialLoad) {
      expressionLoader.loadLookahead();
      this.init();
      this.initialLoad = false;
    }
  },

  postLookaheadWord: function (data, event) {
    expressionLoader.postLookaheadWord(data, event);
  },

  postAnaExpClicked: function (word) {
    expressionLoader.postAnaExpClicked(word);
  },

  updateLookUpTable: function () {
    expressionLoader.updateLookUpTable();
  },

  populateLookUpTable: function (data) {
    expressionLoader.populateLookUpTable(data);
  },

  postToken(word) {
    let wordData = textLineData.createNode(word);
    let request = $.ajax({
      url: "/peng",
      type: "POST",
      data: wordData,
      async: false
    });
    return request;
  },

  updateViewForWord(word) {
    let request = $.when(this.postToken(word));
    // console.log("word", word);
    request.done(function (data) {
      let json = JSON.parse(data);

      if (word == "." || word == "?") {
        let sentences = (viewModel.textAreaStr().trim().slice(0, viewModel.textAreaStr().length) + word)
        // remove extra '.'s when there are consecutive .'s e.g .. = .
        let regexReplaceFullStop = /(^[\.\s]*)|([\s\.]*(?=(\.|\))))|(\s*\([\.\s]*\)\s*\.)|(\s*(?=\())/g;
        sentences = sentences.replace(regexReplaceFullStop, "").trim();

        // remove 
        let regexReplaceQuestionMark = /(^[\?\s]*)|([\s\?]*(?=(\?|\))))|(\s*\([\?\s]*\)\s*\?)|(\s*(?=\())/g;
        sentences = sentences.replace(regexReplaceQuestionMark, "").trim();

        // 
        sentences = sentences.replace(/\.(?!\d)|([^\d])\.(?=\d)/g, '$1.|');
        sentences = sentences.replace(/\?(?!\d)|([^\d])\?(?=\d)/g, '$1?|');

        let sentencesArray = sentences.split("|");
        sentencesArray.pop(); //remove "" at the end of the array

        console.log("sentencesArray", sentencesArray)
        if (sentencesArray.length > 0) {
          textLineData.addSentence((sentencesArray.pop()).trim());
        }

        // console.log("sentencesArray", sentencesArray);
        viewModel.setAsp(json);
        viewModel.setAnswer(json.answer);

        if (viewModel.isDropdownInput ||
          viewModel.isLoadFileInput ||
          viewModel.isCursorInput) {
          viewModel.updateViewForWord(" ");
        }
      }

      if ((json.hasOwnProperty('spelling suggestions') || (json.lookahead.length == 0 && !json.hasOwnProperty('asp')) 
          && !viewModel.isCursorInput)) {
        viewModel.allowInput = false;
        let lAhead = lookaheadObj.createLookaheadTable(lookaheadObj);
        lAhead = lookaheadObj.addStrInHeadForEachCatInLookahead(word, lAhead);
        viewModel.lookaheadObject(lAhead);
        viewModel.anaExp(json.ana);
      } else {
        viewModel.populateLookUpTable(json);
        expressionLoader.loadLookahead();
        viewModel.anaExp(json.ana);
        viewModel.allowInput = true;
      }


      if (textLineData.lastSentenceNodes().length > 1) {
        viewModel.processedInput(textLineData.lastSentenceNodes().join(" "));
      } else if (textLineData.sentences.length > 0) {
        viewModel.processedInput(textLineData.sentences[textLineData.sentences.length - 1]);
      }

      if (json.hasOwnProperty('ana') && word != "." && json.ana.length != 0) {
        let temp = json.ana;
        for (i = 0; i < json.ana.length; i++) {
          if (json.ana[i].length > 1) {
            temp[i] = [temp[i].join(" ")];

          } else {
            temp[i] = json.ana[i];
          }
        }
        viewModel.anaExp(temp);
      }
    });
    console.log("nodes", textLineData.nodes);
  },

  // NAVBAR functions

  // Loads the list of file names in texts folder
  loadFileNames: function () {
    if (this.initialLoad) {
      expressionLoader.loadLookahead();
      this.init();
      this.initialLoad = false;
    }
    navBar.loadFileNames();
  },

  // Load a single file name
  loadFile: function () {
    let loadedFileName = this;
    navBar.loadFile(loadedFileName);
  },

  generateText: function () {
    navBar.generateText();
  },

  saveFile() {
    navBar.saveButton();
  },

  // EVENT HANDLER functions

  // Text submitted
  onSubmit: function () {
    console.log("onSubmit")
    eventHandler.enterKey();
  },

  // Char entered
  onCharInput: function (d, e) {
    return eventHandler.keyUpdate(d, e);
  },

  // Backspace detected
  onBackSpace: function (d, e) {
    let keyVal = e.keyCode;
    if (keyVal == 8) {
      eventHandler.backspace();
    }
  },

  onCursorInput: function (d, e) {
    viewModel.loadLookahead();
    console.log("onCursorInput");
    eventHandler.alertPrevWord();
  },

  // RESULTS section functions

  setAnswer: function (ansData) {
    results.setAnswer(ansData);
  },

  setAsp: function (aspData) {
    results.setAsp(aspData);
  }
};

ko.applyBindings(viewModel);


// Prevent default behavior of the text area html element
(function ($) {
  $(document).ready(function () {
    $("#text_field").keypress(function (e) {
      if (e.keyCode == 13 && !e.shiftKey) {
        e.preventDefault();
        return false;
      }
    });
    $("#text_field").click(function (e) {
      console.log("text field clicked");
      e.preventDefault();
    });
    $("#text_field").keyup(function (e) {
      if (e.keyCode == 8) {
        e.preventDefault();
        return false;
      }
    });
  });
})(jQuery);