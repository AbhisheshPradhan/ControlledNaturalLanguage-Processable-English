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

      $procSpan: $("#proc-span"),

      allowInit: true,
      // CLASSES
      lookahead: Lookahead,
      allowInput: true,

      catTable: ko.observableArray([]),
      anaExp: ko.observableArray([]),

      lookaheadObject: ko.observableArray([]),

      reasonerMode: "normal", // default settings
      inputMode: ko.observable("Text Mode"), //default settings
      speechData: ko.observable(""),

      $loader: $(".loader"),

      textParaList: ko.observableArray([]),

      submitButton: function() {
            KeyHandler.enterKey();
            console.log("Sentences : ", textLineData.sentences);
            console.log("Nodes : ", textLineData.nodes);
      },

      changeToTextMode: function() {
            $("#start_button").hide();
            $("#speech-detected").hide();
            this.inputMode("Text Mode");
            $('.searchbox-div').css('height', "220px");
      },

      changeToSpeechMode: function() {
            $("#start_button").show();
            $("#speech-detected").show();
            this.inputMode("Speech Mode");
            $('.searchbox-div').css('height', "240px");
            init();
            this.init();
      },

      changeToNormal: function() {
            this.reasonerMode = "normal"
      },

      changeToBrave: function() {
            this.reasonerMode = "brave";
      },

      changeToCautious: function() {
            this.reasonerMode = "cautious";
      },

      loadLookahead: function() {
            this.init();  // only needed at first load
            var lookaheadTable = this.lookahead.createLookaheadTable(this.lookahead);
            this.lookaheadObject(lookaheadTable);
      },

      loadLookahead2: function() {
            var lookaheadTable = this.lookahead.createLookaheadTable(this.lookahead);
            this.lookaheadObject(lookaheadTable);
      },

      postWordClicked: function(data, event) {
            clickHelper.postWordClicked(data, event);
            // console.log("postWordClicked");
            // console.log("cursor between word? : ", KeyHandler.cursorBetweenText);
            // console.log("getWords : ", KeyHandler.getWords())
            // console.log("getWordsInTextBox : ", KeyHandler.getWordsInTextBox())
      },

      postAnaExpClicked: function() {
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
      loadFileNames: function() {
            if(viewModel.fileNames().length == 0) {
                  var jsonObj = this.createJsonObject("load", " ", " ", " ", "off", "normal");
                  $.ajax({
                        url : "/peng/",
                        type: "POST",
                        data : jsonObj,
                        success: function(data, textStatus, jqXHR)
                        {
                              var json = JSON.parse(data);
                              fnames = json.filenames;   // .slice(1,  json.filenames.length);
                              viewModel.fileNames(fnames);
                        },
                        error: function (jqXHR, textStatus, errorThrown)
                        {
                              alert("Failed input on loading file names: \n "+errorThrown);
                        }
                  });
            }
      },

      // Load a single file name
      loadFile: function() {
	    // viewModel.$loader.css("visibility", "visible");
            var jsonObj = viewModel.createJsonObject("load", " ", this, " ", "off", "normal");
            var str = JSON.stringify(this);
            str = str.replace(" ", "");
            str = str.replace('"', "");
            str = str.replace('"', "");
            var index = viewModel.fileNames().indexOf(str);
            var fn = viewModel.fileNames();
            // fn.splice(index, 1);
            viewModel.fileNames(fn);
            // console.log(viewModel.fileNames());
            $.ajax({
                  url : "/peng/",
                  type: "POST",
                  data : jsonObj,
                  success: function(data, textStatus, jqXHR)
                  {
                        SuccessHelper.loadSingleTextFile(data, textStatus, jqXHR);

                  },
                  error: function (jqXHR, textStatus, errorThrown)
                  {
                        alert("Failed JSON object input when loading file: \n "+errorThrown);
                  }
            });
            return true;
      },

      // Generates text
      generateText: function() {
	      // viewModel.$loader.css("visibility", "visible");
	      saveTemporary();
            var jsonObj = viewModel.createJsonObject("generate", " ", " ", " ", "off", "normal");
            $.ajax({
                  url : "/peng/",
                  type: "POST",
                  data : jsonObj,
                  success: function(data, textStatus, jqXHR)
                  {
                        SuccessHelper.generateText(data, textStatus, jqXHR);

                  },
                  error: function (jqXHR, textStatus, errorThrown)
                  {
                        alert("Failed to generate text: \n "+errorThrown);
                  }
            });
            return true;
      },

      updateLookUpTable: function() {
            // This concat must be done to avoid unwanted changes in to other data structures
            var tempLookAheadTable = [].concat(this.lookUpTable());
            this.lookUpTable(this.lookahead.filterTable(this.token(), tempLookAheadTable));
      },

      init: function() {
            var lastNodePostedWasBlank = textLineData.nodes[textLineData.nodes.length - 1] != " ";
            var textAreaEmpty = this.textAreaStr().length == 0
            if (textAreaEmpty && lastNodePostedWasBlank) {
                  this.postToken(" ");
                  this.initSentenceLookUp = this.lookUpTable();
                  this.initLookUpObj = this.lookaheadObject();
            }
            else {
                  this.$text_field.val(this.textAreaStr());
            }
      },

      populateLookUpTable: function(data) {
            this.lookahead.setAll(data);
            this.lookUpTable(this.lookahead.getWordTable());
      },

      setAnswer: function(ansData) {
            var ans = "";
            for(var s = 0; s < ansData.length; s++) {
                  ans += ansData[s]+"\n";
            }
            this.answer(ans);
      },

      setAsp: function(aspData) {
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

      postToken2(word) {
            var wordData =  textLineData.createNode(word, this.reasonerMode);
            var request = $.ajax({
                  url : "/peng/",
                  type: "POST",
                  data : wordData,
                  async: this.asyncFlag
            });
            return request;
      },

      postToken(word) {
            // If word in lookahead
            var request = $.when(this.postToken2(word));
            console.log("posted word : ", word);
            request.done(function(data)
            {
                  var json = JSON.parse(data);
                  // console.log(json);

                  if (word == "." || word == "?") {
                        viewModel.setAsp(json);
		            viewModel.setAnswer(json.answer)
                        var para = json.para;
                        var newPara = [];
                        var s = "";
                        for(var z = 0; z < para.length; z++) {
                              if(para[z] == "?" || para[z] == ".") {
                                    s = s.slice(0, s.length-1) + para[z];
                                    newPara.push(s)
                                    s = "";
                              }
                              else if(para[z] == ",") {
                                    s = s.slice(0, s.length-1) + para[z]+" ";
                              }
                              else {
                                    s += para[z] + " ";
                              }
                        }
                        viewModel.textParaList(newPara);
                        viewModel.allowInput = true;
                  }

                  if(json.hasOwnProperty('spelling suggestions') || 
                        (json.lookahead.length == 0 && !json.hasOwnProperty('asp'))) {
                        viewModel.allowInput = false;
                        var lAhead = Lookahead.createLookaheadTable(Lookahead);
                        lAhead = Lookahead.addStrInHeadForEachCatInLookahead(word, lAhead);
                        viewModel.lookaheadObject(lAhead);
		            viewModel.anaExp(json.ana);
                  }
                  else {
                        viewModel.populateLookUpTable(json);
                        viewModel.loadLookahead2(); // MAYBE REMOVE IF
		            viewModel.anaExp(json.ana);
                        viewModel.allowInput = true;
                  }

                  if(json.hasOwnProperty('ana') && word != "." && json.ana.length != 0) {
                        var temp = json.ana;
                        for(i = 0; i < json.ana.length; i++) {
                              if(json.ana[i].length > 1) {
                                    temp[i] = [temp[i].join(" ")];

                              }
                              else {
                                    temp[i] = json.ana[i];
                              }
                        }
                        viewModel.anaExp(temp);
                  }
            });
      },

      loadLookUptableFor(word) {
            // If word in lookahead
            var request = $.when(this.postToken2(word));
            console.log("posted word : ", word);

            request.done(function(data)
            {
                  var json = JSON.parse(data);
                  console.log(json)

                  viewModel.populateLookUpTable(json);
                  viewModel.loadLookahead2(); // MAYBE REMOVE IF
                  viewModel.anaExp(json.ana);
                  viewModel.allowInput = true;
            });
      },

      contains: function(target, arr) {
            for (i = 0; i < arr.length; i++) {
                  if (target == arr[i]) {
                        return true;
                  }
            }
            return false;
      },

      keyHandler: function(d, e) {
            return KeyHandler.keyUpdate(d, e);
      },

      backSpaceHandler: function(d, e) {
            KeyHandler.backspace(d, e);
      },

      createJsonObject: function(emode, token, fname, stext, reason, rmode) {
            var object = {
                  id: -1,
                  inputmode:"text",
                  editmode:emode,
                  token: token,
                  featurestructure:"{ \"cat\" : \" \",  \"wform\" : \" \"}",
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
                  spectext += ('\n'+textLineData.sentences[i]+'\n');
            }

            var saveData = {
                  "id":"-1",
                  "inputmode":"text",
                  "editmode":"save",
                  "token":" ",
                  "featurestructure":"{ \"cat\" : \" \",  \"wform\" : \" \"}",
                  "filename":file_name,
                  "spectext":spectext,
                  "snum":"0",
                  "spos":"0",
                  "reasoner":"off",
                  "reasonermode":"normal"
            }
            $.ajax({
                  url : "/peng/",
                  type: "POST",
                  data : saveData
            });
      }
};

function saveTemporary() {
      var file_name = 'text.tmp';
      if (textLineData.sentences.length > 0) {
            var spectext = "";
            for (var i = 0; i < textLineData.sentences.length; i++) {
                  textLineData.sentences[i] = textLineData.sentences[i].split('\r').join('');
                  spectext += ('\n'+textLineData.sentences[i]+'\n');
            }

            var saveData = {
                  "id":"-1",
                  "inputmode":"text",
                  "editmode":"save",
                  "token":" ",
                  "featurestructure":"{ \"cat\" : \" \",  \"wform\" : \" \"}",
                  "filename":file_name,
                  "spectext":spectext,
                  "snum":"0",
                  "spos":"0",
                  "reasoner":"off",
                  "reasonermode":"normal"
            }
            $.ajax({
                  url : "/peng/",
                  type: "POST",
                  data : saveData
            });
      } else { 
            alert('CNL Text is empty.');
            throw ' '; 
      }
};

var addToLexicon = function(cat, wform, vform, num) {
    var idNum = textLineData.nodes.length ;
    var fs = FeatureStructure.createFS(cat, wform, vform, num);
      var addData = {
            "id": idNum,
            "inputmode":"text",
            "editmode":"add",
            "token": wform,
	      "featurestructure":fs,
            "filename":" ",
            "spectext":" ",
            "snum":textLineData.sentences.length+1,
            "spos":textLineData.getSpos() -1,
            "reasoner":"off",
            "reasonermode":"normal"
      }

      $.ajax({
            url : "/peng/",
            type: "POST",
            data : addData
      });
}

$( document ).ready(function() {
      $(".dropdown-menu").delegate("li", "click", function() {
            var str = JSON.stringify($(this).html());
            var addRegex = /Add/i
            var saveRegex = /Save/i
            var loadRegex = /Load/i

            var notFile = !(addRegex.test(str) || saveRegex.test(str) );

            if(notFile) {
                  $(this).addClass("active").siblings().removeClass("active");
            }

      });

      // FOR MENU COLOUR
      $('ul.dropdown-menu [data-toggle=dropdown]').on('click', function(event) {
            event.preventDefault();
            event.stopPropagation();
            $(this).parent().siblings().removeClass('open');
            $(this).parent().toggleClass('open');
      });

      // NEED FOR TABS
      $('.collapse').on('shown.bs.collapse', function(){
            $(this).parent().find(".glyphicon-plus-sign").removeClass("glyphicon-plus-sign").addClass("glyphicon-minus-sign");
      }).on('hidden.bs.collapse', function(){
            $(this).parent().find(".glyphicon-minus-sign").removeClass("glyphicon-minus-sign").addClass("glyphicon-plus-sign");
      });


      /// for asp toggle (dev mode)
      $("[name='my-checkbox']").bootstrapSwitch();
      $('input[name="my-checkbox"]').on('switchChange.bootstrapSwitch', function(event, state) {
            if (state) {
                  viewModel.asp(viewModel.bigAsp);
            }
            else {
                  viewModel.asp(viewModel.smallAsp);
            }
            viewModel.aspState = state;
      });
});
