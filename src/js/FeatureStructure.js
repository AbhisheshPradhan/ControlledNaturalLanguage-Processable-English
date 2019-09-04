// Rewritten by Rolf Schwitter: 2019-02-03

var FeatureStructure = {

	createFS: function (cat, wform, vform, num) {
		var fs = new Object()
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
		var str = JSON.stringify(fs);
		return str;
	}
}
