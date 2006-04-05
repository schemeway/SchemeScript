/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript;

import gnu.mapping.Environment;

import java.net.*;
import java.util.*;

import kawa.standard.*;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.preference.*;
import org.eclipse.jface.util.*;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.plugin.*;
import org.osgi.framework.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.interpreter.*;
import org.schemeway.plugins.schemescript.parser.*;
import org.schemeway.plugins.schemescript.preferences.*;

/**
 * The main plugin class to be used in the desktop.
 */
public class SchemeScriptPlugin extends AbstractUIPlugin {
    private static final String CONF_USER_SCM = "conf/user.scm";
    public final static String PLUGIN_NS = "org.schemeway.plugins.schemescript";

    private static final String INTERNAL_INTERPRETER_NAME = "internal";
    private static final String INTERNAL_INTERPRETER_PREF = PLUGIN_NS + ".interpreter.name"; 
    
    //The shared instance.
    private static SchemeScriptPlugin plugin;
    //Resource bundle.
    private ResourceBundle resourceBundle;

    private SchemeTextTools textTools;
    private InterpreterType mCurrentInterpreter;
    
    IPropertyChangeListener propertyChangedListener = null;
    
    /**
     * The constructor.
     */
    public SchemeScriptPlugin() {
        super();
        plugin = this;
        try {
            resourceBundle = ResourceBundle.getBundle("resources");
        }
        catch (MissingResourceException x) {
            resourceBundle = null;
            logException("Unable to locate resource bundle!", x);
        }
    }

    /**
     * This method is called upon plug-in activation
     */
    public void start(BundleContext context) throws Exception {
        super.start(context);

        Scheme.registerEnvironment();

        loadConfigFile(CONF_USER_SCM);
        
        textTools = new SchemeTextTools(new ColorManager());
        
        if (propertyChangedListener == null) {
            propertyChangedListener = new IPropertyChangeListener() {
                public void propertyChange(PropertyChangeEvent event) {
                    IPreferenceStore store = getPreferenceStore();
                    if (event.getProperty().startsWith(SchemeLexicalExtensionsPreferences.PREFIX)) {
                        initializeScanner(store);
                    }
                }
            };
            getPreferenceStore().addPropertyChangeListener(propertyChangedListener);
        }
    }
    
    protected void loadConfigFile(String filename) {
    	if (filename != null) {
    		KawaProxy.loadFile(find (new Path(filename)). toString());
    	}
    }
    
    /**
     * This method is called when the plug-in is stopped
     */
    public void stop(BundleContext context) throws Exception {
        super.stop(context);
        if (propertyChangedListener != null) {
            getPreferenceStore().removePropertyChangeListener(propertyChangedListener);
        }
    }

    /**
     * Returns the shared instance.
     */
    public static SchemeScriptPlugin getDefault() {
        return plugin;
    }

    /**
     * Returns the string from the plugin's resource bundle, or 'key' if not
     * found.
     */
    public static String getResourceString(String key) {
        ResourceBundle bundle = SchemeScriptPlugin.getDefault().getResourceBundle();
        try {
            return (bundle != null) ? bundle.getString(key) : key;
        }
        catch (MissingResourceException e) {
            return key;
        }
    }

    /**
     * Returns the plugin's resource bundle,
     */
    public ResourceBundle getResourceBundle() {
        return resourceBundle;
    }

    public SchemeTextTools getTextTools() {
        return textTools;
    }
    
    
    public Interpreter getInterpreter() {
        if (mCurrentInterpreter == null) 
            mCurrentInterpreter = createDefaultInterpreter();
        return mCurrentInterpreter.getInterpreter();
    }
    
    public void setInterpreter(InterpreterType type) {
        Assert.isNotNull(type);
        mCurrentInterpreter = type;
        getPreferenceStore().setValue(INTERNAL_INTERPRETER_PREF, type.getID());
    }
    
    public InterpreterType getCurrentInterpreterType() {
        if (mCurrentInterpreter == null)
            mCurrentInterpreter = createDefaultInterpreter();
        return mCurrentInterpreter;
    }
    
    public Interpreter getInternalInterpreter() {
        InterpreterType[] types = InterpreterSupport.getTypes();
        for (int i=0; i<types.length; i++) {
            if (types[i].getID().equals(INTERNAL_INTERPRETER_NAME)) {
                return types[i].getInterpreter();
            }
        }
        return null;
    }
    
    protected InterpreterType createDefaultInterpreter() {
        String ID = getPreferenceStore().getString(INTERNAL_INTERPRETER_PREF);
        
        InterpreterType[] types = InterpreterSupport.getTypes();
        if (types.length == 0)
            return null;
        
        for(int index = 0; index < types.length; index++) {
            if (types[index].getID().equals(ID)) {
                return types[index];
            }
        }
        return types[0];
    }
  
    
    public static void logException(String message, Throwable exception) {
        IStatus status = new Status(IStatus.ERROR, 
                                    getDefault().getBundle().getSymbolicName(),
                                    IStatus.OK,
                                    message,
                                    exception);
        getDefault().getLog().log(status);
    
    }

    protected void initializeDefaultPreferences(IPreferenceStore store) {
        store.setDefault(INTERNAL_INTERPRETER_PREF, INTERNAL_INTERPRETER_NAME);
        SchemePreferences.initializeDefaults(store);
        CommentPreferences.initializeDefaults(store);
        ColorPreferences.initializeDefaults(store);
        SyntaxPreferences.initializeDefaults(store);
        IndentationPreferences.initializeDefaults(store);
        SchemeLexicalExtensionsPreferences.initializeDefaults(store);
        RemoteInterpreterPreferences.initializeDefaults(store);
        initializeScanner(store);
    }
    
    private void initializeScanner(IPreferenceStore store) {
        SchemeScannerUtilities.setBracketsAreParentheses(store.getBoolean(SchemeLexicalExtensionsPreferences.SQUARE_BRACKETS));
        SchemeScannerUtilities.setDashInIdentifiers(store.getBoolean(SchemeLexicalExtensionsPreferences.DASH_IN_IDS));
    }
}
