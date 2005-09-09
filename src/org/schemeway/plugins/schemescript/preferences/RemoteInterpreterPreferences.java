/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.*;

import org.schemeway.plugins.schemescript.*;

public class RemoteInterpreterPreferences extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".remoteInterp.";
    public final static String INTERPRETER_HOST = PREFIX + "host";
    public final static String INTERPRETER_PORT = PREFIX + "port";

    public RemoteInterpreterPreferences() {
        super("Remove interpreter preferences", GRID);
    }

    public void init(IWorkbench workbench) {
    }

    protected IPreferenceStore doGetPreferenceStore() {
        return SchemeScriptPlugin.getDefault().getPreferenceStore();
    }

    protected void initializeDefaultPreferences(IPreferenceStore store) {
        initializeDefaults(store);
    }

    public static void initializeDefaults(IPreferenceStore store) {
        store.setDefault(INTERPRETER_HOST, "localhost");
        store.setDefault(INTERPRETER_PORT, "5156");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors()
     */
    protected void createFieldEditors() {
        initializeDefaultPreferences(getPreferenceStore());
        StringFieldEditor hostEditor =  new StringFieldEditor(INTERPRETER_HOST, "Hostname:", getFieldEditorParent());
        hostEditor.setEmptyStringAllowed(false);
        hostEditor.setValidateStrategy(StringFieldEditor.VALIDATE_ON_KEY_STROKE);
        
        IntegerFieldEditor portEditor =  new IntegerFieldEditor(INTERPRETER_PORT, "Port:", getFieldEditorParent());
        portEditor.setValidateStrategy(IntegerFieldEditor.VALIDATE_ON_KEY_STROKE);
        portEditor.setValidRange(1, 32767);
        
        addField(hostEditor);
        addField(portEditor);
    }

    public static String getInterpreterHost() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        return store.getString(RemoteInterpreterPreferences.INTERPRETER_HOST);
    }
    
    public static int getInterpreterPort() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        return store.getInt(RemoteInterpreterPreferences.INTERPRETER_PORT);
    }
}