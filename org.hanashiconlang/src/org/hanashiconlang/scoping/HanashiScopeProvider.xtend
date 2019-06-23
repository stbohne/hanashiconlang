/*
 * generated by Xtext 2.17.0
 */
package org.hanashiconlang.scoping

import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.xtext.scoping.IScope
import org.hanashiconlang.hanashi.GlossLexeme
import org.hanashiconlang.hanashi.HanashiPackage
import org.eclipse.xtext.EcoreUtil2
import org.hanashiconlang.hanashi.Lexeme
import org.hanashiconlang.hanashi.Gloss
import org.eclipse.xtext.scoping.Scopes
import org.hanashiconlang.hanashi.Lexicon
import org.hanashiconlang.hanashi.TagRef
import org.hanashiconlang.hanashi.Tag
import org.eclipse.xtext.naming.QualifiedName

/**
 * This class contains custom scoping description.
 * 
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#scoping
 * on how and when to use it.
 */
class HanashiScopeProvider extends AbstractHanashiScopeProvider {
	override IScope getScope(EObject context, EReference reference) {
		if (context instanceof GlossLexeme &&
			reference == HanashiPackage.Literals.GLOSS_LEXEME__LEXEME) {
			val gloss = EcoreUtil2.getContainerOfType(context, Gloss)
			val lexemes = EcoreUtil2.getAllContentsOfType(EcoreUtil2.getRootContainer(context), Lexeme).filter([
				EcoreUtil2.getContainerOfType(it, Lexicon).language == gloss.language 
			])
			return Scopes.scopeFor(lexemes, QualifiedName.wrapper[Lexeme it | it.meta.id], IScope.NULLSCOPE)
		} else if(context instanceof TagRef &&
			reference == HanashiPackage.Literals.TAG_REF__TAG) {
			val tags = EcoreUtil2.getAllContentsOfType(EcoreUtil2.getRootContainer(context), Tag)
			return Scopes.scopeFor(tags, QualifiedName.wrapper[Tag it | it.meta.id ], IScope.NULLSCOPE)
		} else
			return super.getScope(context, reference);
	}
}
