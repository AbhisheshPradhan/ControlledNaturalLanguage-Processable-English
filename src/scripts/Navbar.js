let navBar = {
  loadFileNames: function () {
    if (viewModel.fileNames().length == 0) {
      let jsonObj = this._createJsonObject("load", " ", " ", " ", "off", "normal");
      $.ajax({
        url: "/peng",
        type: "POST",
        data: jsonObj,
        success: function (data) {
          let json = JSON.parse(data);
          let filenames = json.filenames;
          viewModel.fileNames(filenames);
        },
        error: function (jqXHR, textStatus, errorThrown) {
          alert("Failed input on loading file names: \n " + errorThrown);
        }
      });
    }
  },

  saveButton: function () {
    let file_name = prompt("Please enter file name");
    if (file_name != null) {
      viewModel.$loading.show();
      let spectext = "";
      for (let i = 0; i < textLineData.sentences.length; i++) {
        textLineData.sentences[i] = textLineData.sentences[i].split('\r').join('');
        spectext += ('\n' + textLineData.sentences[i] + '\n');
      }

      let saveData = {
        "id": "-1",
        "inputmode": "text",
        "editmode": "save",
        "token": " ",
        "featurestructure": "{ \"cat\" : \" \",  \"wform\" : \" \"}",
        "filename": file_name + ".txt",
        "spectext": spectext,
        "snum": "0",
        "spos": "0",
        "reasoner": "off",
        "reasonermode": "normal"
      }
      $.ajax({
        url: "/peng",
        type: "POST",
        data: saveData,
        success: function () {
          viewModel.$loading.hide();
          alert("File saved: " + file_name);
        },
        error: function (jqXHR, textStatus, errorThrown) {
          viewModel.$loading.hide();
          alert("Failed to save file: \n " + errorThrown);
        }
      });
    }
  },

  // Load a single file name
  loadFile: function (loadedFileName) {
    let self = this;

    let jsonObj = this._createJsonObject("load", " ", loadedFileName, " ", "off", "normal");

    // read the text file and send them as nodes instead..
    if (viewModel.isEndOfSentence || textLineData.nodes[textLineData.nodes.length - 1] == " ") {
      viewModel.$loading.show();
      $.ajax({
        url: "/peng",
        type: "POST",
        data: jsonObj,
        success: function (data) {
          let json = JSON.parse(data);
          let nodes = self._formatToReadableInput(json.spectext);
          for (let i = 0; i < nodes.length; i++) {
            if (nodes[i] != "") {
              let x = i;
              if (nodes[i] == "." || nodes[i] == "?") {
                viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1) + nodes[i] + " ");
              } else {
                viewModel.textAreaStr(viewModel.textAreaStr() + nodes[i] + " ");
              }
              viewModel.isLoadFileInput = true;
              viewModel.$text_field.val(viewModel.textAreaStr());
              viewModel.updateViewForWord(nodes[i]);
              i = x;
            }
          }
          // viewModel.isLoadFileInput = true;
          viewModel.updateViewForWord(" ");
          // Removing the last " " node as we also need to re update the view using " "
          if (textLineData.nodes[textLineData.nodes.length - 1] == " " && textLineData.nodes[textLineData.nodes.length - 2] == " ") {
            textLineData.nodes.pop();
          }
          viewModel.$loading.hide();
          viewModel.isLoadFileInput = false;
        },
        error: function (jqXHR, textStatus, errorThrown) {
          viewModel.$loading.hide();
          viewModel.isLoadFileInput = false;
          alert("Failed JSON object input when loading file: \n " + errorThrown);
        }
      });
    } else {
      alert("Complete sentence before loading file.");
    }

    return true;
  },

  // Generates text
  generateText: function () {
    let self = this;
    this.saveTemporary();
    let jsonObj = this._createJsonObject("generate", " ", " ", " ", "off", "normal");
    viewModel.$loading.show();
    $.ajax({
      url: "/peng",
      type: "POST",
      data: jsonObj,
      success: function (data) {
        let json = JSON.parse(data);
        console.log("json", json);

        // console.log("spectext: ", json.spectext);

        viewModel.textList.removeAll();

        let gsentence = "";
        let gnodes = self._formatToReadableInput(json.gentext);

        // console.log("gentext: ", json.gentext);

        for (i = 0; i < gnodes.length; i++) {
          let x = i;
          if (gnodes[i] == "." || gnodes[i] == "?") {
            gsentence = gsentence.slice(0, gsentence.length - 1);
            gsentence += gnodes[i];
            viewModel.textList.push(gsentence);
            gsentence = "";
          } else {
            gsentence += gnodes[i] + " ";
          }
          i = x;
        }

        // console.log("sentences", textLineData.sentences);
        // console.log("CNL text list", viewModel.textList());

        viewModel.setAsp(json);
        viewModel.setAnswer(json.answer);
        viewModel.updateViewForWord(" ");
        if (textLineData.nodes[textLineData.nodes.length - 1] == " " && textLineData.nodes[textLineData.nodes.length - 2] == " ") {
          textLineData.nodes.pop();
        }

        // console.log("nodes", textLineData.nodes);
        viewModel.$loading.hide();
      },
      error: function (jqXHR, textStatus, errorThrown) {
        viewModel.$loading.hide();
        alert("Failed to generate text: \n " + errorThrown);
      }
    });
    // console.log("nodes", textLineData.nodes);
    return true;
  },

  saveTemporary: function () {
    var file_name = 'text.tmp';
    if (viewModel.isEndOfSentence || textLineData.nodes[textLineData.nodes.length - 1] == " ") {
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
        url: "/peng",
        type: "POST",
        data: saveData
      });
    } else {
      alert('CNL Text is empty.');
      throw ' ';
    }
  },

  // Helper function
  _formatToReadableInput: function (input) {
    // FORMATS TO READABLE INPUT
    input = input.replace(/%(.)*/g, '');
    input = input.split('\r').join('');
    input = input.split('\n').join('');
    input = input.replace(/\/\*.*\*\//, '');
    input = input.split('\r').join('');
    input = input.split('\n').join('');
    input = input.split('  ').join('');
    input = input.split('.').join(' . ');
    input = input.split('?').join(' ? ');
    input = input.split(', ').join(' , ');
    input = input.split(" ");
    input.pop();
    return input;
  },

  _createJsonObject: function (emode, token, fname, stext, reason, rmode) {
    let object = {
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
}