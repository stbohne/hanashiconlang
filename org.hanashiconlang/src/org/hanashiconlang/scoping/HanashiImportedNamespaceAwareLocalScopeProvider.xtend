package org.hanashiconlang.scoping

import org.eclipse.xtext.scoping.impl.ImportedNamespaceAwareLocalScopeProvider
import org.eclipse.emf.ecore.EObject

class HanashiImportedNamespaceAwareLocalScopeProvider extends ImportedNamespaceAwareLocalScopeProvider {
    
    override getImportedNamespace(EObject object) {
        super.getImportedNamespace(object) + ".*"
    }
    
}