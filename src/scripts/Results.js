var results = {
    setAnswer: function (ansData) {
        var ans = "";
        for (var s = 0; s < ansData.length; s++) {
            ans += ansData[s] + "\n";
        }
        viewModel.answer(ans);
    },

    setAsp: function (aspData) {
        var asp = aspData.asp;
        var clingo = aspData.reasoner;
        viewModel.bigAsp = asp;
        var index = viewModel.bigAsp.search("% -----------------------------");
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
