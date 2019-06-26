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
import java.util.Arrays
import com.google.common.collect.Iterables
import org.hanashiconlang.hanashi.GlossSkip

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
		richString2String(l.entries?.findFirst[e| 
			e.type=="form"
		]?.text, lang)
	}
	def CharSequence glossString(Morpheme l, Language lang) {
		richString2String(l.entries?.findFirst[e| 
			e.type=="gloss" && e.language == lang
		]?.text, lang)
	}
	def CharSequence translationString(Morpheme l, Language lang) {
		richString2String(l.entries?.findFirst[e| 
			e.type=="translation" && e.language == lang
		]?.text, lang)
	}

	override void doGenerate(Resource resource, IFileSystemAccess2 fsa, IGeneratorContext context) {
		val lang = resource.allContents.filter(Language).head
		val baseName = resource.URI.trimFileExtension.lastSegment
		fsa.generateFile(baseName + '.html', '''
			<html>
			<head>
			<style>
			a:link { text-decoration: none; }
			a:hover { text-decoration: underline; }
			.gloss-info { font-size:66%; line-height:20%; }
			</style>
			</head>
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

	def generateRichString(RichString s, Language lang) {
		if (s != null)
			'''�FOR e: s.expressions��generateRichStringExpression(e, lang)��ENDFOR�'''
		else 
			''
	}
	dispatch def generateRichStringExpression(RichStringLiteral l, Language lang) {
		l.value
	}
	dispatch def generateRichStringExpression(Gloss g, Language lang) { 
		val classes = #["gloss"] + (richString2String(g.html.class_, lang)?.toString?.split(" ") ?: newArrayOfSize(0))
		'''
		<table style="border:0" class="�classes.join(" ")�">
			�FOR l: g.lines�
    		    <tr class="gloss-words">�
    		    	generateGlossWordsText(l.words, lang)
    	    	�</tr>�
                IF (l.words.exists[items.exists[it instanceof GlossMorpheme]])
                �<tr class="gloss-info">�
		    		generateGlossWordsInfo(l.words, lang)
	    		�</tr>�
                ENDIF�
		    �ENDFOR�
		</table>'''
	}
	
	def richString2String(RichString s, Language lang) {
		if (s != null)
			'''�FOR e: s.expressions��richStringExpression2String(e, lang)��ENDFOR�'''
		else
			''
	}
	dispatch def richStringExpression2String(RichStringLiteral l, Language lang) {
		l.value
	}
	dispatch def richStringExpression2String(Gloss g, Language lang) {
		glossWords2String(g.lines.get(0).words, lang)
	}
	
	def generateGlossWordsText(EList<GlossWord> gws, Language lang) 
		'''�FOR gw: gws�<td style="padding:0.1em" �
		  IF gw.skips.size > 0�colspan="�gw.skips.size + 1�"�ENDIF�>�
		  FOR gi: gw.items.indexed��
		      IF gi.key > 0 && gi.value instanceof GlossMorpheme && 
		              gw.items.get(gi.key - 1) instanceof GlossMorpheme��ENDIF��
              generateGlossText(gi.value, lang)��
          ENDFOR
		  �</td>�ENDFOR�'''
	dispatch def generateGlossText(GlossMorpheme gl, Language lang) { 
		val classes = #["morpheme-ref"] + 
		    (richString2String(gl.morpheme.html.refclass, lang)?.toString?.split(" ") ?: newArrayOfSize(0)) + 
			(richString2String(gl.morpheme.lexicon.morphemerefclass, lang)?.toString?.split(" ") ?: newArrayOfSize(0))
		'''<a href="#morpheme$�gl.morpheme.language.name�$�gl.morpheme.name�" title="�gl.morpheme.translationString(lang)�" classes="�classes.join(" ")�">�gl.morpheme.formString(lang)�</a>'''
	}
	dispatch def generateGlossText(GlossString gs, Language lang) {
		gs.string
	} 
	dispatch def generateGlossText(GlossRichString gs, Language lang) {
		generateRichString(gs.string, lang)
	} 
	def generateGlossWordsInfo(EList<GlossWord> gws, Language lang) 
		'''�FOR gw: gws�<td>�FOR gi: gw.items.indexed��
                IF gi.key > 0 && gi.value instanceof GlossMorpheme && 
                        gw.items.get(gi.key - 1) instanceof GlossMorpheme�.�ENDIF��
                generateGlossInfo(gi.value, lang)��ENDFOR�</td>�
        ENDFOR�'''
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
	dispatch def glossItem2String(GlossSkip gs, Language lang) {
		''
	}
	
	dispatch def generateDeclaration(Section s, int depth, Language lang) {
		val classes = #["section"] + (richString2String(s.html.class_, lang)?.toString?.split(" ") ?: newArrayOfSize(0))
		'''<h�depth� id="section$�s.name�" class="�classes.join(" ")�">�generateRichString(s.title, lang)�</h1>
		�generateRichString(s.text, lang)�
		�FOR p: s.parts�
		�generateDeclaration(p, depth + 1, lang)�
		�ENDFOR�'''
	}
	dispatch def generateDeclaration(Lexicon l, int depth, Language lang) {
		val classes = #["lexicon"] + (richString2String(l.html.class_, lang)?.toString?.split(" ") ?: newArrayOfSize(0))
        '''<h�depth��IF !l.name.nullOrEmpty� id="lexicon$�l.name�"�ENDIF� class="�classes.join(" ")�">Lexicon for �generateRichString(l.language.title, lang)�</h1>
        �FOR m: l.morphemes�
        �generateMorpheme(m, depth + 1, lang)�
        �ENDFOR�'''
	}
	dispatch def generateDeclaration(Taxonomy t, int depth, Language lang) {
        val classes = #["taxonomy"] + (richString2String(t.html.class_, lang)?.toString?.split(" ") ?: newArrayOfSize(0))
        '''<h�depth��IF !t.name.nullOrEmpty� id="taxonomy$�t.name�"�ENDIF� class="�classes.join(" ")�">Taxonomy</h1>
        �FOR tx: t.taxons�
        �generateTaxon(tx, depth + 1, lang)�
        �ENDFOR�'''
	}
	dispatch def generateDeclaration(Language l, int depth, Language lang) {
		""
	}
	
	def generateMorpheme(Morpheme m, int depth, Language lang) {
		val classes = #["morpheme"] + (richString2String(m.html.class_, lang)?.toString?.split(" ") ?: newArrayOfSize(0))
		'''<div class="�classes.join(" ")�">
		<b id="morpheme$�m.language.name�$�m.name�">�FOR e: m.entries.filter[e| e.type == "form"] SEPARATOR " / "��richString2String(e.text, lang)��ENDFOR�</b>
		�FOR t: m.taxons SEPARATOR " "�<a href="#taxon$�t.target.name�">�t.target.name�</a>�ENDFOR�
		�FOR e: m.entries.filter[type == "translation"] BEFORE "<p>Translations:<dl>\n" AFTER "</dl></p>"�
		<dd>�generateRichString(e.language.title, lang)�</dd>
		<dt>�generateRichString(e.text, lang)�<dt/>
		�ENDFOR�
		�FOR e: m.entries.filter[type == "derived"] BEFORE "<p>Derived from:<dl>\n" AFTER "</dl></p>"�
		<dd>�generateRichString(e.language.title, lang)�</dd>
		<dt>�generateRichString(e.text, lang)�<dt/>
		�ENDFOR�
		</div>'''
	}

	def generateTaxon(Taxon t, int depth, Language lang) {
        val classes = #["taxon"] + (richString2String(t.html.class_, lang)?.toString?.split(" ") ?: newArrayOfSize(0))
        val parent = EcoreUtil2.getContainerOfType(t.eContainer, Taxon)
		'''<div class="�classes.join(" ")�">
		<b id="taxon$�t.name�">Taxon �t.name�</b>
		�IF parent !== null� <a href="#taxon$�parent.name�">�parent.name�</a>�ENDIF�
		�generateRichString(t.text, lang)�
		�FOR tx: t.taxons��generateTaxon(tx, depth + 1, lang)��ENDFOR�
		</div>'''
	}
}
