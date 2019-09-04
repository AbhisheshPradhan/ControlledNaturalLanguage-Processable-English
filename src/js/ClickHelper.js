
var clickHelper = {
      postWordClicked: function (data, event) {
            if (typeof data.add != "undefined") {
                  if (confirm('Are you sure you want to add "' + data.add + '" to the temporary vocabulary?')) {
                        var catIndex = 0;
                        var cat = $(event.target).parent().parent().parent().siblings().html();
                        for (i = 0; i < Lookahead.cats.length; i++) {
                              if (cat == Lookahead.cats[i]) {
                                    catIndex = i;
                                    break;
                              }
                        }
                        var num = Lookahead.nums[catIndex];
                        var vform = (num == " ") ? "bse" : "fin";
                        var wform = data.add;
                        addToLexicon(cat, wform, vform, num);
                        var dataArr1 = wform.split(" ");
                        var str = viewModel.textAreaStr().toString();
                        str = str.replace(/^\s+|\s+$/g, "");
                        var pos = str.lastIndexOf(" ");
                        str = str.substring(0, pos + 1);
                        viewModel.textAreaStr(str);
                        var s1;
                        for (s1 = 0; s1 < dataArr1.length; s1++) {
                              viewModel.updateViewForWord(viewModel.token() + dataArr1[s1]);
                              viewModel.textAreaStr(viewModel.textAreaStr() + dataArr1[s1] + " ");
                              viewModel.firstIndexOfCurrentWord = viewModel.textAreaStr().length;
                              viewModel.loadLookahead();
                        }
                  } else {
                        return false;
                  }
            } else if (viewModel.allowInput) {
                  var dataArr = data.split(" ");
                  var s;
                  for (s = 0; s < dataArr.length; s++) {
                        viewModel.updateViewForWord(viewModel.token() + dataArr[s]);
                        if (dataArr[s] == "." || dataArr[s] == "?") {
                              viewModel.token(dataArr[s]);
                              viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1) + dataArr[s]);
                        } else if (data == ",") {
                              viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1) + dataArr[s] + " ");
                        } else {
                              viewModel.textAreaStr(viewModel.textAreaStr() + dataArr[s] + " ");
                              viewModel.firstIndexOfCurrentWord = viewModel.textAreaStr().length;
                              viewModel.loadLookahead();
                        }
                  }
                  if (!viewModel.allowInput) {
                        viewModel.token(viewModel.token() + dataArr[0]);
                  }
            }
      },

      postAnaExpClicked: function (words) {
            var wordA = "" + words;
            var word = wordA.split(" ");
            if (viewModel.lookUpTable().indexOf(word[0]) != -1) {
                  if (word.length > 1) {
                        for (k = 0; k < word.length; k++) {
                              viewModel.updateViewForWord(word[k]);
                              viewModel.textAreaStr(viewModel.textAreaStr() + word[k] + " ");
                        }
                  } else {
                        viewModel.updateViewForWord(word[0]);
                        viewModel.textAreaStr(viewModel.textAreaStr() + word[0] + " ");
                  }
                  viewModel.loadLookahead();
            } else {
                  alert("The anaphoric expression \"" + word + "\" is not a valid token and will not be submitted. " + "Please try another input.");
            }
      }
}
