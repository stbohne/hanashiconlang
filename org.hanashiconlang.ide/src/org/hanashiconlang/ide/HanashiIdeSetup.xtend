/*
 * generated by Xtext 2.17.0
 */
package org.hanashiconlang.ide

import com.google.inject.Guice
import org.eclipse.xtext.util.Modules2
import org.hanashiconlang.HanashiRuntimeModule
import org.hanashiconlang.HanashiStandaloneSetup

/**
 * Initialization support for running Xtext languages as language servers.
 */
class HanashiIdeSetup extends HanashiStandaloneSetup {

	override createInjector() {
		Guice.createInjector(Modules2.mixin(new HanashiRuntimeModule, new HanashiIdeModule))
	}
	
}