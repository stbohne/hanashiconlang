package org.hanashiconlang

import org.eclipse.xtext.EcoreUtil2
import org.hanashiconlang.hanashi.Morpheme
import org.hanashiconlang.hanashi.Lexicon
import org.hanashiconlang.hanashi.Document
import org.hanashiconlang.hanashi.Language

class HanashiExtensions {
	static def lexicon(Morpheme l) {
		EcoreUtil2.getContainerOfType(l, Lexicon)
	}
	static def language(Morpheme l) {
		l.lexicon.language
	}
}