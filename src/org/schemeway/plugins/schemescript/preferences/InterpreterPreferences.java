/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.preferences;

import java.io.*;
import java.util.regex.*;

import org.eclipse.jface.preference.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;

public class InterpreterPreferences extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
    
    private static class RegexFieldEditor extends StringFieldEditor {
        public RegexFieldEditor(String name, String label, Composite container) {
            super(name, label, container);
            setValidateStrategy(StringFieldEditor.VALIDATE_ON_KEY_STROKE);
        }
        
        protected boolean doCheckState() {
            boolean valid = true;
            try {
                Pattern.compile(getStringValue());
            }
            catch (PatternSyntaxException exception) {
                valid = false;
            }
            return valid;
        }
    }
    
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".interpreter.";
    public final static String INTERPRETER_NAME = PREFIX + "name";
    public final static String INTERPRETER_CMDLINE = PREFIX + "cmdline";
    public final static String INTERPRETER_DIR = PREFIX + "directory";
    public final static String INTERPRETER_ERROR_REGEX = PREFIX + "errorRegexp";
    public final static String INTERPRETER_FILENAME_GROUP = PREFIX + "filenameGroup";
    public final static String INTERPRETER_LINENO_GROUP = PREFIX + "lineNoGroup";
    public final static String INTERPRETER_LINK_GROUP = PREFIX + "linkGroup";

    Pattern mErrorPattern = null;
    
    public InterpreterPreferences() {
        super("Interpreter preferences", GRID);
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
        store.setDefault(INTERPRETER_NAME, "Scheme");
        store.setDefault(INTERPRETER_CMDLINE, "scheme");
        store.setDefault(INTERPRETER_DIR, "");
        store.setDefault(INTERPRETER_ERROR_REGEX, "");
        store.setDefault(INTERPRETER_FILENAME_GROUP, -1);
        store.setDefault(INTERPRETER_LINENO_GROUP, -1);
        store.setDefault(INTERPRETER_LINK_GROUP, -1);
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors()
     */
    protected void createFieldEditors() {
        initializeDefaultPreferences(getPreferenceStore());
        addField(new StringFieldEditor(INTERPRETER_NAME, "Interpreter name:", getFieldEditorParent()));
        addField(new StringFieldEditor(INTERPRETER_CMDLINE, "Command line:", getFieldEditorParent()));
        addField(new DirectoryFieldEditor(INTERPRETER_DIR, "Working directory:", getFieldEditorParent()));
        
        addField(new RegexFieldEditor(INTERPRETER_ERROR_REGEX, "Error Regexp:", getFieldEditorParent()));

        IntegerFieldEditor editor = null;
        editor = new IntegerFieldEditor(INTERPRETER_FILENAME_GROUP, "Filename group:", getFieldEditorParent());
        editor.setValidRange(-1, 50);
        addField(editor);
        
        editor = new IntegerFieldEditor(INTERPRETER_LINENO_GROUP, "Line number group:", getFieldEditorParent());
        editor.setValidRange(-1, 50);
        addField(editor);
        
        editor = new IntegerFieldEditor(INTERPRETER_LINK_GROUP, "Error link group:", getFieldEditorParent());
        editor.setValidRange(-1, 50);
        addField(editor);
    }

    public static String getCommandLine() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        return store.getString(INTERPRETER_CMDLINE);
    }
    
    public static String getInterpreterName() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        return store.getString(INTERPRETER_NAME);
    }
    
    public static File getWorkingDirectory() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        String dir = store.getString(INTERPRETER_DIR);
        
        if (dir == null || dir.equals(""))
            return null;
        else {
            return new File(dir);
        }
    }
    
    public static String getErrorRegexp() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        return store.getString(INTERPRETER_ERROR_REGEX);
    }
    
    public static int getFilenameGroup() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        return store.getInt(INTERPRETER_FILENAME_GROUP);
    }
    
    public static int getLineNumberGroup() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        return store.getInt(INTERPRETER_LINENO_GROUP);
    }

    public static int getLinkGroup() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        return store.getInt(INTERPRETER_LINK_GROUP);
    }
}