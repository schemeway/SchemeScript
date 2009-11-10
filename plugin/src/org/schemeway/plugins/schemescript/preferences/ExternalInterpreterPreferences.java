/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.preferences;

import java.io.*;
import java.util.regex.*;

import org.eclipse.core.runtime.*;
import org.eclipse.core.variables.*;
import org.eclipse.jface.preference.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;

public class ExternalInterpreterPreferences extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

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

	private static class WorkingDirectoryFieldEditor extends DirectoryFieldEditor {
		public WorkingDirectoryFieldEditor(String name, String label, Composite container) {
			super(name, label, container);
			setValidateStrategy(StringFieldEditor.VALIDATE_ON_KEY_STROKE);
		}

		protected boolean doCheckState() {
			boolean valid = true;
			String text = getTextControl().getText();

			if (text.indexOf("${") >= 0) {
				try {
					IStringVariableManager stringVariableManager = VariablesPlugin.getDefault()
							.getStringVariableManager();
					stringVariableManager.validateStringVariables(text);
				}
				catch (CoreException exception) {
					this.setErrorMessage(exception.getMessage());
					valid = false;
				}
			}
			else {
				valid = super.doCheckState();
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
	public final static String INTERPRETER_SAVE_PID = PREFIX + "savesPID";

	Pattern mErrorPattern = null;

	public ExternalInterpreterPreferences() {
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
		store.setDefault(INTERPRETER_SAVE_PID, false);
	}

	protected void createFieldEditors() {
		initializeDefaultPreferences(getPreferenceStore());
		addField(new StringFieldEditor(INTERPRETER_NAME, "Interpreter name:", getFieldEditorParent()));
		addField(new StringFieldEditor(INTERPRETER_CMDLINE, "Command line:", getFieldEditorParent()));
		addField(new WorkingDirectoryFieldEditor(INTERPRETER_DIR, "Working directory:", getFieldEditorParent()));

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

		addField(new BooleanFieldEditor(INTERPRETER_SAVE_PID, "Interpreter saves PID", getFieldEditorParent()));
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

		IStringVariableManager stringVariableManager = VariablesPlugin.getDefault().getStringVariableManager();
		try {
			dir = stringVariableManager.performStringSubstitution(dir);
		}
		catch (CoreException e) {
			SchemeScriptPlugin.logException("Unable to perform variable substitution on working directory.", e);
			return null;
		}

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

	public static boolean getSavesPID() {
		IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
		return store.getBoolean(INTERPRETER_SAVE_PID);
	}
}