/*
 * generated by Xtext 2.17.0
 */
package org.hanashiconlang.validation

import org.eclipse.xtext.validation.Check
import org.hanashiconlang.hanashi.Call
import org.hanashiconlang.generator.HanashiFunctions
import org.eclipse.xtext.EcoreUtil2
import org.hanashiconlang.hanashi.Document
import static extension org.hanashiconlang.HanashiExtensions.*
import org.hanashiconlang.generator.HanashiRenderer

/**
 * This class contains custom validation rules. 
 *
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation
 */
class HanashiValidator extends AbstractHanashiValidator {
	
//	public static val INVALID_NAME = 'invalidName'
//
//	@Check
//	def checkGreetingStartsWithCapital(Greeting greeting) {
//		if (!Character.isUpperCase(greeting.name.charAt(0))) {
//			warning('Name should start with a capital', 
//					HanashiPackage.Literals.GREETING__NAME,
//					INVALID_NAME)
//		}
//	}
	
	@Check
	def checkCall(Call c) {
		val renderer = new HanashiRenderer(EcoreUtil2.getContainerOfType(c, Document).language)
		val func = c.function
		try {
			HanashiFunctions.getDeclaredField(renderer.generateRichString(func, false).toString)
		} catch (NoSuchFieldException exc) {
			error('''The function '�func�' does not exist''', c.function, null)
		}
	}
}
