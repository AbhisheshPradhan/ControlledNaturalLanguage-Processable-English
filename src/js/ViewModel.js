/**
*   Author: Rudz Evan Bernardez, Macquarie University
*   File Name: ViewModel.js
*
*/

var viewModel = {
      $text_field: $("#text_field"),
      textAreaStr: ko.observable(""), // For display
      result: ko.observable(""),
      firstIndexOfCurrentWord: 0,
      token: ko.observable(""),
      textList: ko.observableArray([]),
      smallAsp: "",
      bigAsp: "",
      aspState: false,
      lookahead: Lookahead,
      allowInput: true,
      catTable: ko.observableArray([]),
      anaExp: ko.observableArray([]),
      lookaheadObject: ko.observableArray([]),
      reasonerMode: "normal", // default settings
      inputMode: ko.observable("Text Mode"), //default settings
      $loader: $(".loader"),
      textParaList: ko.observableArray([]),

      submitButton: function () {
            KeyHandler.enterKey();
      },

      loadLookahead: function () {
            this.init();  // only needed at first load
            var lookaheadTable = this.lookahead.createLookaheadTable(this.lookahead);
            this.lookaheadObject(lookaheadTable);
      },

      loadLookahead2: function () {
            var lookaheadTable = this.lookahead.createLookaheadTable(this.lookahead);
            this.lookaheadObject(lookaheadTable);
      },

      postWordClicked: function (data, event) {
            clickHelper.postWordClicked(data, event);
      },

      postAnaExpClicked: function () {
            clickHelper.postAnaExpClicked(this);
      },

      lookUpTable: ko.observableArray([]),
      asp: ko.observable(''),
      answer: ko.observable(''),
      currentInitialLookUpTable: [],
      initSentenceLookUp: [],
      asyncFlag: false,
      fileNames: ko.observableArray([]),
      
      //WIDGETS
      loadFileNames: function () {
            if (viewModel.fileNames().length == 0) {
                  var jsonObj = this.createJsonObject("load", " ", " ", " ", "off", "normal");
                  $.ajax({
                        url: "/peng/",
                        type: "POST",
                        data: jsonObj,
                        success: function (data) {
                              var json = JSON.parse(data);
                              var filenames = json.filenames;
                              viewModel.fileNames(filenames);
                        },
                        error: function (jqXHR, textStatus, errorThrown) {
                              alert("Failed input on loading file names: \n " + errorThrown);
                        }
                  });
            }
      },

      // Load a single file name
      loadFile: function () {
            var loadedFileName = this;
            var jsonObj = viewModel.createJsonObject("load", " ", loadedFileName, " ", "off", "normal");
            $.ajax({
                  url: "/peng/",
                  type: "POST",
                  data: jsonObj,
                  success: function (data, textStatus, jqXHR) {
                        SuccessHelper.loadSingleTextFile(data, textStatus, jqXHR);
                  },
                  error: function (jqXHR, textStatus, errorThrown) {
                        alert("Failed JSON object input when loading file: \n " + errorThrown);
                  }
            });
            return true;
      },

      // Generates text
      generateText: function () {
            saveTemporary();
            var jsonObj = viewModel.createJsonObject("generate", " ", " ", " ", "off", "normal");
            $.ajax({
                  url: "/peng/",
                  type: "POST",
                  data: jsonObj,
                  success: function (data, textStatus, jqXHR) {
                        SuccessHelper.generateText(data, textStatus, jqXHR);
                  },
                  error: function (jqXHR, textStatus, errorThrown) {
                        alert("Failed to generate text: \n " + errorThrown);
                  }
            });
            return true;
      },

      updateLookUpTable: function () {
            // This concat must be done to avoid unwanted changes in to other data structures
            var tempLookAheadTable = [].concat(this.lookUpTable());
            this.lookUpTable(this.lookahead.filterTable(this.token(), tempLookAheadTable));
      },

      init: function () {
            var lastNodePostedWasBlank = textLineData.nodes[textLineData.nodes.length - 1] != " ";
            var textAreaEmpty = this.textAreaStr().length == 0
            if (textAreaEmpty && lastNodePostedWasBlank) {
                  this.updateViewForWord(" ");
                  this.initSentenceLookUp = this.lookUpTable();
                  this.initLookUpObj = this.lookaheadObject();
            }
            else {
                  this.$text_field.val(this.textAreaStr());
            }
      },

      populateLookUpTable: function (data) {
            this.lookahead.setAll(data);
            this.lookUpTable(this.lookahead.getWordTable());
      },

      setAnswer: function (ansData) {
            var ans = "";
            for (var s = 0; s < ansData.length; s++) {
                  ans += ansData[s] + "\n";
            }
            this.answer(ans);
      },

      setAsp: function (aspData) {
            var asp = aspData.asp;
            var clingo = aspData.reasoner;
            this.bigAsp = asp;
            var index = this.bigAsp.search("% -----------------------------");
            this.smallAsp = this.bigAsp.slice(0, index);

            if (this.aspState) {
                  this.asp(this.bigAsp);
            }
            else {
                  this.asp(this.smallAsp);
            }

            this.result(clingo);
            this.allowInput = true;
      },

      postToken(word) {
            var wordData = textLineData.createNode(word, this.reasonerMode);
            var request = $.ajax({
                  url: "/peng/",
                  type: "POST",
                  data: wordData,
                  async: false
            });
            return request;
      },

      updateViewForWord(word) {
            // If word in lookahead
            var request = $.when(this.postToken(word));
            request.done(function (data) {
                  var json = JSON.parse(data);
                  if (word == "." || word == "?") {
                        viewModel.setAsp(json);
                        viewModel.setAnswer(json.answer)
                        var para = json.para;
                        var newPara = [];
                        var s = "";
                        for (var z = 0; z < para.length; z++) {
                              if (para[z] == "?" || para[z] == ".") {
                                    s = s.slice(0, s.length - 1) + para[z];
                                    newPara.push(s)
                                    s = "";
                              } else if (para[z] == ",") {
                                    s = s.slice(0, s.length - 1) + para[z] + " ";
                              } else {
                                    s += para[z] + " ";
                              }
                        }
                        viewModel.textParaList(newPara);
                        viewModel.allowInput = true;
                  }

                  if (json.hasOwnProperty('spelling suggestions') || (json.lookahead.length == 0 && !json.hasOwnProperty('asp'))) {
                        viewModel.allowInput = false;
                        var lAhead = Lookahead.createLookaheadTable(Lookahead);
                        lAhead = Lookahead.addStrInHeadForEachCatInLookahead(word, lAhead);
                        viewModel.lookaheadObject(lAhead);
                        viewModel.anaExp(json.ana);
                  } else {
                        viewModel.populateLookUpTable(json);
                        viewModel.loadLookahead2(); // MAYBE REMOVE IF
                        viewModel.anaExp(json.ana);
                        viewModel.allowInput = true;
                  }

                  if (json.hasOwnProperty('ana') && word != "." && json.ana.length != 0) {
                        var temp = json.ana;
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
      },

      keyHandler: function (d, e) {
            return KeyHandler.keyUpdate(d, e);
      },

      backSpaceHandler: function (d, e) {
            KeyHandler.backspace(d, e);
      },

      createJsonObject: function (emode, token, fname, stext, reason, rmode) {
            var object = {
                  id: -1,
                  inputmode: "text",
                  editmode: emode,
                  token: token,
                  featurestructure: "{ \"cat\" : \" \",  \"wform\" : \" \"}",
                  filename: fname,
                  spectext: stext,
                  snum: -1,
                  spos: -1,
                  reasoner: reason,
                  reasonermode: rmode
            }
            return object;
      }
};

ko.applyBindings(viewModel);

function saveButton() {
      var file_name = prompt("Please enter your name");
      if (file_name != null) {
            var spectext = "";
            for (var i = 0; i < textLineData.sentences.length; i++) {
                  textLineData.sentences[i] = textLineData.sentences[i].split('\r').join('');
                  spectext += ('\n' + textLineData.sentences[i] + '\n');
            }

            var saveData = {
                  "id": "-1",
                  "inputmode": "text",
                  "editmode": "save",
                  "token": " ",
                  "featurestructure": "{ \"cat\" : \" \",  \"wform\" : \" \"}",
                  "filename": file_name,
                  "spectext": spectext,
                  "snum": "0",
                  "spos": "0",
                  "reasoner": "off",
                  "reasonermode": "normal"
            }
            $.ajax({
                  url: "/peng/",
                  type: "POST",
                  data: saveData
            });
      }
};


function saveTemporary() {
      var file_name = 'text.tmp';
      if (textLineData.sentences.length > 0) {
            var spectext = "";
            for (var i = 0; i < textLineData.sentences.length; i++) {
                  textLineData.sentences[i] = textLineData.sentences[i].split('\r').join('');
                  spectext += ('\n' + textLineData.sentences[i] + '\n');
            }

            var saveData = {
                  "id": "-1",
                  "inputmode": "text",
                  "editmode": "save",
                  "token": " ",
                  "featurestructure": "{ \"cat\" : \" \",  \"wform\" : \" \"}",
                  "filename": file_name,
                  "spectext": spectext,
                  "snum": "0",
                  "spos": "0",
                  "reasoner": "off",
                  "reasonermode": "normal"
            }
            $.ajax({
                  url: "/peng/",
                  type: "POST",
                  data: saveData
            });
      } else {
            alert('CNL Text is empty.');
            throw ' ';
      }
};



var addToLexicon = function (cat, wform, vform, num) {
      var idNum = textLineData.nodes.length;
      var fs = FeatureStructure.createFS(cat, wform, vform, num);
      var addData = {
            "id": idNum,
            "inputmode": "text",
            "editmode": "add",
            "token": wform,
            "featurestructure": fs,
            "filename": " ",
            "spectext": " ",
            "snum": textLineData.sentences.length + 1,
            "spos": textLineData.getSpos() - 1,
            "reasoner": "off",
            "reasonermode": "normal"
      }

      $.ajax({
            url: "/peng/",
            type: "POST",
            data: addData
      });
}
