/*
 * generated by Xtext 2.17.0
 */
package org.hanashiconlang


/**
 * Initialization support for running Xtext languages without Equinox extension registry.
 */
class HanashiStandaloneSetup extends HanashiStandaloneSetupGenerated {

	def static void doSetup() {
		new HanashiStandaloneSetup().createInjectorAndDoEMFRegistration()
	}
}
