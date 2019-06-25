/*
 * generated by Xtext 2.17.0
 */
package org.hanashiconlang.generator

import java.util.stream.IntStream
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.generator.AbstractGenerator
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.generator.IGeneratorContext
import org.hanashiconlang.hanashi.Choice
import org.hanashiconlang.hanashi.Document
import org.hanashiconlang.hanashi.Gloss
import org.hanashiconlang.hanashi.GlossMorpheme
import org.hanashiconlang.hanashi.GlossWord
import org.hanashiconlang.hanashi.Language
import org.hanashiconlang.hanashi.Morpheme
import org.hanashiconlang.hanashi.NonTerminal
import org.hanashiconlang.hanashi.Optional
import org.hanashiconlang.hanashi.Repeated
import org.hanashiconlang.hanashi.Section
import org.hanashiconlang.hanashi.Sequence
import org.hanashiconlang.hanashi.Syntax
import org.hanashiconlang.hanashi.Terminal
import org.hanashiconlang.hanashi.Weighted

import static extension java.util.stream.IntStream.*
import org.hanashiconlang.hanashi.Lexicon
import org.hanashiconlang.hanashi.Taxonomy
import org.hanashiconlang.hanashi.Taxon
import org.eclipse.xtext.EcoreUtil2
import org.hanashiconlang.hanashi.RichString
import org.hanashiconlang.hanashi.RichStringLiteral
import org.hanashiconlang.hanashi.GlossString
import org.hanashiconlang.hanashi.GlossRichString

/**
 * Generates code from your model files on save.
 * 
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#code-generation
 */
class HanashiGenerator extends AbstractGenerator {
//	
//	dispatch def IntStream getTerminals(Terminal prod) {
//		prod.text.codePoints
//	}
//	dispatch def IntStream getTerminals(NonTerminal prod) {
//		getTerminals(prod.target.production)
//	}
//	dispatch def IntStream getTerminals(Weighted prod) {
//		IntStream.empty
//	}
//	dispatch def IntStream getTerminals(Optional prod) {
//		getTerminals(prod.inner)
//	}
//	dispatch def IntStream getTerminals(Repeated prod) {
//		getTerminals(prod.inner)
//	}
//	dispatch def IntStream getTerminals(Choice prod) {
//		getTerminals(prod.left).concat(getTerminals(prod.right))
//	}
//	dispatch def IntStream getTerminals(Sequence prod) {
//		prod.seq.map[p| getTerminals(p) ].stream().flatMap[s| s.boxed() ].mapToInt[v| v]
//	}
	
	def lexicon(Morpheme l) {
		EcoreUtil2.getContainerOfType(l, Lexicon)
	}
	def language(Morpheme l) {
		l.lexicon.language
	}
	
	def CharSequence formString(Morpheme l, Language lang) {
		richString2String(l.entries.findFirst[e| 
			e.type=="\\form"
		].text, lang)
	}
	def CharSequence glossString(Morpheme l, Language lang) {
		richString2String(l.entries.findFirst[e| 
			e.type=="\\gloss" && e.language == lang
		].text, lang)
	}
	def CharSequence translationString(Morpheme l, Language lang) {
		richString2String(l.entries.findFirst[e| 
			e.type=="\\translation" && e.language == lang
		].text, lang)
	}
//	def <T> strip(Iterable<T> list, Class<? extends T> clazz) {
//		var list_ = list
//		while (!list_.nullOrEmpty && clazz.isInstance(list_.head))
//			list_ = list_.drop(1)
//		while (!list_.nullOrEmpty && clazz.isInstance(list_.last))
//			list_ = list_.take(list_.size - 1)
//		list_
//	}
//
	override void doGenerate(Resource resource, IFileSystemAccess2 fsa, IGeneratorContext context) {
		val lang = resource.allContents.filter(Language).head
		val baseName = resource.URI.trimFileExtension.lastSegment
		fsa.generateFile(baseName + '.html', '''
			<html>
			<body>
			�FOR d: resource.contents.filter(Document)�
				�FOR p: d.parts�
				�generateDeclaration(p, 1, lang)�
				�ENDFOR�
			�ENDFOR�
			</body>
			</html>
		''')
		
	}

	def generateRichString(RichString s, Language lang)
		'''�FOR e: s.expressions��generateRichStringExpression(e, lang)��ENDFOR�'''
	dispatch def generateRichStringExpression(RichStringLiteral l, Language lang) {
		l.value
	}
	dispatch def generateRichStringExpression(Gloss g, Language lang) { 
		val classes = #["gloss"] + g.html.class_.split(" ").map["usr-" + it]
		'''
		<span style="display:inline-table" class="�classes.join(" ")�">
			�FOR l: g.lines�
		    <span style="display:table-row; text-align:left">�generateGlossWordsText(l.words, lang)�</span>
		    <span style="display:table-row; font-size:66%; line-height:20%; text-align:center">�generateGlossWordsInfo(l.words, lang)�</span>
		    �ENDFOR�
		</span>'''
	}
	
	def richString2String(RichString s, Language lang)
		'''�FOR e: s.expressions��richStringExpression2String(e, lang)��ENDFOR�'''
	dispatch def richStringExpression2String(RichStringLiteral l, Language lang) {
		l.value
	}
	dispatch def richStringExpression2String(Gloss g, Language lang) {
		glossWords2String(g.lines.get(0).words, lang)
	}
	
	def generateGlossWordsText(EList<GlossWord> gws, Language lang) 
		'''�FOR gw: gws�<span style="display:table-cell; padding:0.1em">�FOR gi: gw.items��generateGlossText(gi, lang)��ENDFOR�</span>�ENDFOR�'''
	dispatch def generateGlossText(GlossMorpheme gl, Language lang) { 
		val classes = #["morpheme-ref"] + gl.morpheme.html.refclass.split(" ").map["usr-" + it] + 
			gl.morpheme.lexicon.morphemerefclasses.map["usr-" + it]
		'''<a style="text-decoration: none" href="#morpheme$�gl.morpheme.language.name�$�gl.morpheme.name�" title="�gl.morpheme.translationString(lang)�" classes="�classes.join(" ")�"">�gl.morpheme.formString(lang)�</a>'''
	}
	dispatch def generateGlossText(GlossString gs, Language lang) {
		gs.string
	} 
	dispatch def generateGlossText(GlossRichString gs, Language lang) {
		generateRichString(gs.string, lang)
	} 
	def generateGlossWordsInfo(EList<GlossWord> gws, Language lang) 
		'''�FOR gw: gws�<span style="display:table-cell">�FOR gi: gw.items��generateGlossInfo(gi, lang)��ENDFOR�</span>�ENDFOR�'''
	dispatch def generateGlossInfo(GlossMorpheme gl, Language lang) 
		'''�gl.morpheme.glossString(lang)�'''
	dispatch def generateGlossInfo(GlossString gs, Language lang) {
		gs.string
	}
	dispatch def generateGlossInfo(GlossRichString gs, Language lang) {
		richString2String(gs.string, lang)
	}
	def glossWords2String(EList<GlossWord> gws, Language lang)
		'''�FOR gw: gws SEPARATOR " "��FOR gi: gw.items��glossItem2String(gi, lang)��ENDFOR��ENDFOR�'''
	dispatch def glossItem2String(GlossMorpheme gm, Language lang) {
		gm.morpheme.glossString(lang)
	}
	dispatch def glossItem2String(GlossString gs, Language lang) {
		gs.string
	}
	dispatch def glossItem2String(GlossRichString gs, Language lang) {
		richString2String(gs.string, lang)
	}
	
	dispatch def generateDeclaration(Section s, int depth, Language lang) {
		val classes = #["section"] + s.html.class_.split(" ").map["usr-" + it]
		'''<h�depth� id="section$�s.name�" class="�classes.join(" ")�">�s.title�</h1>
		�generateRichString(s.text, lang)�
		�FOR p: s.parts�
		�generateDeclaration(p, depth + 1, lang)�
		�ENDFOR�'''
	}
	dispatch def generateDeclaration(Lexicon l, int depth, Language lang) {
		""
	}
	dispatch def generateDeclaration(Taxonomy t, int depth, Language lang) {
		""
	}
//	dispatch def generateDeclaration(Morpheme l, int depth, Language lang) {
//		val classes = #["morpheme"] + l.classes.classes.map["usr-" + it]
//		'''<div class="�classes.join(" ")�">
//		<b id="morpheme$�l.language.name�$�l.meta.id�">�FOR e: l.entries.filter[e| e.type == "\\form"] SEPARATOR " / "��freeTexts2String(e.texts, lang)��ENDFOR�</b>
//		�FOR t: l.meta.taxons SEPARATOR " "�<a href="#taxon$�t.target.meta.id�">�t.target.meta.id�</a>�ENDFOR�
//		�FOR e: l.entries.filter[e| e.type=="\\translation"] BEFORE "<p>Translations:<dl>\n" AFTER "</dl></p>"�
//		<dd>�e.language.title�</dd>
//		<dt>�generateFreeTexts(e.texts, lang)�<dt/>
//		�ENDFOR�
//		�FOR e: l.entries.filter[e| e.type=="\\derived"] BEFORE "<p>Derived from:<dl>\n" AFTER "</dl></p>"�
//		<dd>�e.language.title�</dd>
//		<dt>�generateFreeTexts(e.texts, lang)�<dt/>
//		�ENDFOR�
//		</div>'''
//	}
//	dispatch def generateDeclaration(Lexicon l, int depth, Language lang) {
//		val classes = #["lexicon"] + l.classes.classes.map["usr-" + it]
//		'''<div class="�classes.join(" ")�">
//		<h�depth�>Lexicon for �l.language.title�</h�depth�>
//		�FOR lx: l.morphemes��generateDeclaration(lx, depth + 1, lang)��ENDFOR�
//		</div>'''
//	}
//	dispatch def generateDeclaration(Taxonomy t, int depth, Language lang) {
//		val classes = #["taxonomy"] + t.classes.classes.map["usr-" + it]
//		'''<div class="�classes.join(" ")�">
//		<h�depth�>Taxonomy</h�depth�>
//		�FOR tx: t.taxons��generateDeclaration(tx, depth + 1, lang)��ENDFOR�
//		</div>'''
//	}
//	dispatch def generateDeclaration(Taxon t, int depth, Language lang) {
//		val classes = #["taxon"] + t.classes.classes.map["usr-" + it]
//		'''<div class="�classes.join(" ")�">
//		<h�depth� id="taxon$�t.meta.id�">Taxon �t.meta.id�</h�depth�>
//		�generateFreeTexts(t.texts, lang)�
//		�FOR tx: t.taxons��generateDeclaration(tx, depth + 1, lang)��ENDFOR�
//		</div>'''
//	}
}
