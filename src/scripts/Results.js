let results = {
  setAnswer: function (ansData) {
    let ans = "";
    for (let s = 0; s < ansData.length; s++) {
      ans += ansData[s] + "\n";
    }
    viewModel.answer(ans);
  },

  setAsp: function (aspData) {
    let asp = aspData.asp;
    let clingo = aspData.reasoner;
    viewModel.bigAsp = asp;
    let index = viewModel.bigAsp.search("% -----------------------------");
    viewModel.smallAsp = viewModel.bigAsp.slice(0, index);

    if (viewModel.aspState) {
      viewModel.asp(viewModel.bigAsp);
    } else {
      viewModel.asp(viewModel.smallAsp);
    }

    viewModel.result(clingo);
    viewModel.allowInput = true;
  }
}
