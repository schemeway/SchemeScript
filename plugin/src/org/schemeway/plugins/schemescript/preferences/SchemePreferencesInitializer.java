/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.core.runtime.preferences.*;
import org.eclipse.jface.preference.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.parser.*;

/**
 * @author SchemeWay Project.
 *
 */
public class SchemePreferencesInitializer extends AbstractPreferenceInitializer {

	public void initializeDefaultPreferences() {
		IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
		
        store.setDefault(SchemeScriptPlugin.INTERNAL_INTERPRETER_PREF, SchemeScriptPlugin.INTERNAL_INTERPRETER_NAME);
        SchemePreferences.initializeDefaults(store);
        CommentPreferences.initializeDefaults(store);
        ColorPreferences.initializeDefaults(store);
        SyntaxPreferences.initializeDefaults(store);
        IndentationPreferences.initializeDefaults(store);
        SchemeLexicalExtensionsPreferences.initializeDefaults(store);
        RemoteInterpreterPreferences.initializeDefaults(store);
        SchemeScannerUtilities.initializeScanner(store);
	}

}
