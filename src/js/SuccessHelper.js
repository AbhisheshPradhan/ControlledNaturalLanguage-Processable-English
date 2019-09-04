/**
*     Contains helper functions for when AJAX requests is successful
*     Modified by Rolf Schwitter: 2019-01-04
*/

var SuccessHelper = {
      loadSingleTextFile: function (data) {
            var json = JSON.parse(data);
            var nodes = this._formatToReadableInput(json.spectext);
            var sentence = "";
            textLineData['sentences'] = [];
            viewModel.textList([]);
            for (i = 0; i < nodes.length; i++) {
                  var x = i;
                  if (nodes[i] == "." || nodes[i] == "?") {
                        sentence = sentence.slice(0, sentence.length - 1);
                        sentence += nodes[i];
                        textLineData.addSentence(sentence);
                        viewModel.textList.push(sentence);
                        sentence = "";
                  }
                  else {
                        sentence += nodes[i] + " ";
                  }
                  i = x;
            }
            viewModel.setAsp(json);
            viewModel.setAnswer(json.answer);
            viewModel.updateViewForWord(" ");
            viewModel.$loader.css("visibility", "hidden");
      },

      generateText: function (data) {
            var json = JSON.parse(data);
            var sentence = "";
            var nodes = this._formatToReadableInput(json.spectext);
            textLineData['sentences'] = [];
            viewModel.textList([]);

            for (i = 0; i < nodes.length; i++) {
                  var x = i;
                  if (nodes[i] == "." || nodes[i] == "?") {
                        sentence = sentence.slice(0, sentence.length - 1);
                        sentence += nodes[i];
                        textLineData.addSentence(sentence);
                        sentence = "";
                  } else {
                        sentence += nodes[i] + " ";
                  }
                  i = x;
            }

            var gsentence = "";
            var gnodes = this._formatToReadableInput(json.gentext);
            for (i = 0; i < gnodes.length; i++) {
                  var x = i;
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
            viewModel.setAsp(json);
            viewModel.setAnswer(json.answer);
            viewModel.updateViewForWord(" ");
            viewModel.$loader.css("visibility", "hidden");
      },

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
            return input.split(" ");
      }
}
