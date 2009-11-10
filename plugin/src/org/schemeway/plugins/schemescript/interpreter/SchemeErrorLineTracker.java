/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import java.util.regex.*;

import org.eclipse.core.resources.*;
import org.eclipse.debug.ui.console.*;
import org.eclipse.debug.ui.console.IConsole;
import org.eclipse.jface.text.*;
import org.eclipse.jface.util.*;
import org.eclipse.ui.console.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class SchemeErrorLineTracker implements IConsoleLineTracker, IPropertyChangeListener {
    private static final int NO_GROUP = -1;
    private Pattern mErrorPattern = null;
    private int     mFilenameGroup = NO_GROUP;
    private int     mLineNoGroup = NO_GROUP;
    private int     mLinkGroup = NO_GROUP;
    
    IConsole mConsole;

    public void init(IConsole console) {
        mConsole = console;
        SchemeScriptPlugin.getDefault().getPreferenceStore().addPropertyChangeListener(this);
        setup();
    }
    

    public void propertyChange(PropertyChangeEvent event) {
        if (event.getProperty().startsWith(ExternalInterpreterPreferences.PREFIX)) {
            setup();
        }
    }
    
    private void setup() {
        String regex = ExternalInterpreterPreferences.getErrorRegexp();
        if (regex != null) {
            mErrorPattern  = Pattern.compile(regex);
            mFilenameGroup = ExternalInterpreterPreferences.getFilenameGroup();
            mLineNoGroup   = ExternalInterpreterPreferences.getLineNumberGroup();
            mLinkGroup     = ExternalInterpreterPreferences.getLinkGroup();
        }
        else {
            mErrorPattern  = null;
            mFilenameGroup = NO_GROUP;
            mLineNoGroup   = NO_GROUP;
            mLinkGroup     = NO_GROUP;
        }
    }
    
    private boolean isPatternSetup() {
        if (mErrorPattern == null || mLineNoGroup == NO_GROUP || mLinkGroup == NO_GROUP || mFilenameGroup == NO_GROUP)
            return false;
        else
            return true;
    }
    
    
    public void lineAppended(IRegion line) {
        if (!isPatternSetup())
            return;
        
        try {
            IDocument document = mConsole.getDocument();
            String text = document.get(line.getOffset(), line.getLength());
            Matcher matcher = mErrorPattern.matcher(text);
            
            if (matcher.matches()) {
                int linkOffset = line.getOffset() + matcher.start(mLinkGroup);
                int linkEnd = line.getOffset() + matcher.end(mLinkGroup);
                int linkLength = linkEnd - linkOffset;
                
                String filename = text.substring(matcher.start(mFilenameGroup), matcher.end(mFilenameGroup));
                String lineStr = text.substring(matcher.start(mLineNoGroup), matcher.end(mLineNoGroup));
                if (filename == null || filename.equals("") || lineStr == null || lineStr.equals(""))
                	return;

                int lineno = Integer.parseInt(lineStr);
                
                IFile file = findFile(filename);
                if (file != null) {
                    mConsole.addLink((IHyperlink) new FileLink(file, null, -1, -1, lineno), linkOffset, linkLength);
                }
            }
        }
        catch (BadLocationException exception) {
        }
        catch (IndexOutOfBoundsException exception) {
            SchemeScriptPlugin.logException("Invalid regex group for interpreter error messages", exception);
        }
    }
    
    private IFile findFile(String filename) {
        return SchemeScriptTools.findFile(filename, ExternalInterpreterPreferences.getWorkingDirectory().getAbsolutePath());
    }

    public void dispose() {
    }
}
