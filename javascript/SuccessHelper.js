/**
*     Contains helper functions for when AJAX requests is successful
*     Modified by Abhishesh Pradhan: 29/07/2019
*/


var SuccessHelper = {

      /*
      Formats the data to readable input
      Filters out any other character other than alphabets and contructs an array of words

      Input:
            data: a large set of string containing symbols, break-points, punctuation, spaces, etc
      Output: 
            data: an array of words contained in the input
      */
      formatToReadableInput (data) {
            data = data.replace(/%(.)*/g, '');
            data = data.split('\r').join('');
            data = data.split('\n').join('');
            data = data.replace(/\/\*.*\*\//, '');
            data = data.split('\r').join('');
            data = data.split('\n').join('');
            data = data.split('  ').join('');
            data = data.split('.').join(' . ');
            data = data.split('?').join(' ? ');
            data = data.split(', ').join(' , ');
            return data.split(" ");
      },


      /*
      contructs sentence using nodes passed
      add sentence to textList (CNL text list) and display
      if mode == 1, add sentence to textLineData

      Input:
            nodes: array of words
            mode: flag to add sentence to textLineData
      */
      addSentences (nodes, mode) {
            var sentence = "";
            textLineData['sentences'] = [];
            viewModel.textList([]);

            for (i = 0; i < nodes.length; i++) {
                  var x = i;
                  if (nodes[i] == "." || nodes[i] == "?") {
                        sentence = sentence.slice(0, sentence.length - 1);
                        sentence += nodes[i];
                        if(mode == 1) {
                              textLineData.addSentence(sentence);
                        }
                        viewModel.textList.push(sentence);
                        sentence = "";
                  }
                  else {
                        sentence += nodes[i] + " ";
                  }
                  i = x;
            }
      },

      setupViewModel(json) {
            viewModel.setAsp(json);
            viewModel.setAnswer(json.answer);
            viewModel.postToken(" ");
            viewModel.$loader.css("visibility", "hidden");
      },

      loadSingleTextFile: function (data, textStatus, jqXHR) {
            var json = JSON.parse(data);
            var s = json.spectext;
            var nodes = SuccessHelper.formatToReadableInput(s);
            SuccessHelper.addSentences(nodes, 1);
            //viewModel.anaExp(json.ana);
            SuccessHelper.setupViewModel(json);
      },

      generateText: function (data, textStatus, jqXHR) {
            var json = JSON.parse(data);
            var s = json.spectext;
            var nodes = SuccessHelper.formatToReadableInput(s);

            SuccessHelper.addSentences(nodes, 1);

            var g = json.gentext;
            var gnodes = SuccessHelper.formatToReadableInput(g);

            SuccessHelper.addSentences(gnodes, 0);
            SuccessHelper.setupViewModel(json);
      },

      postTokenForLoading: function (data) {
            var json = JSON.parse(data);
            viewModel.populateLookUpTable(json);

            if (word == "." || word == "?") {
                  viewModel.setAsp(json.asp);
                  viewModel.allowInput = true;
            }

            if (json.hasOwnProperty('spelling suggestions') || (json.lookahead.length == 0 && !json.hasOwnProperty('asp'))) {
                  viewModel.allowInput = false;
                  alert('"' + word + '" is not a valid token. Please go back and re-enter a token or add to lexicon');
                  viewModel.addButton("Add");
            }
            else {
                  viewModel.allowInput = true;
            }

            if (json.hasOwnProperty('ana') && word != ".") {
                  viewModel.anaExp(json.ana);
            }
      }
}
