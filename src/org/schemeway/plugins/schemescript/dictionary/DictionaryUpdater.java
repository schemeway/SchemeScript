/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.dictionary;

import gnu.mapping.*;

import java.lang.reflect.*;
import java.util.*;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.operation.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.interpreter.*;

/**
 * User dictionary. Holds all the symbols defined by user code. All Scheme files
 * in all open projects are considered.
 * 
 * TODO Remove entries from removed resources (like when closing a project) TODO
 * Use a better technique to disable dictionaries...
 */
public class DictionaryUpdater implements IResourceChangeListener {
	private List mSchemeExtensions;

	private List mPendingResources = Collections.synchronizedList(new LinkedList());
	private Hashtable mEntries = new Hashtable();

	private DictionaryUpdater(String extensions) {
		initializeExtensions(extensions);
	}

	public void dispose() {
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
		mEntries = null;
		mPendingResources = null;
	}

	public static DictionaryUpdater createInstance(String extensions) {
		DictionaryUpdater instance = new DictionaryUpdater(extensions);
		instance.initialize();
		return instance;
	}

	private void initializeExtensions(String extensions) {
		StringTokenizer tokenizer = new StringTokenizer(extensions, ",");
		mSchemeExtensions = new LinkedList();
		while (tokenizer.hasMoreElements()) {
			mSchemeExtensions.add(tokenizer.nextToken());
		}
	}

	private void initialize() {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		final IProject[] projects = root.getProjects();
		try {
			for (int i = 0; i < projects.length; i++) {
				IProject project = projects[i];
				collectSchemeSources(project);
			}
//			processPendingResources(true);
		}
		catch (Exception exception) {
			SchemeScriptPlugin.logException("Error while updating user dictionary", exception);
		}
	}

	private void collectSchemeSources(IProject project) {
		try {
			IResource[] members = project.members();
			for (int elementIndex = 0; elementIndex < members.length; elementIndex++) {
				scanElement(members[elementIndex]);
			}
		}
		catch (Exception exception) {
		}
	}

	private void scanElement(IResource resource) throws CoreException {
		if (resource instanceof IFile) {
			IFile file = (IFile) resource;
			if (isSchemeSourceFile(file)) {
				synchronized (mPendingResources) {
					mPendingResources.add(file);
				}
			}
		}
		else if (resource instanceof IContainer) {
			IContainer container = (IContainer) resource;
			IResource[] members = container.members();
			for (int elementIndex = 0; elementIndex < members.length; elementIndex++) {
				scanElement(members[elementIndex]);
			}
		}
	}

	public void processPendingResources(boolean displayProgress) {
		if (!mPendingResources.isEmpty()) {
			try {
				final IFile[] files;
				synchronized (mPendingResources) {
					files = new IFile[mPendingResources.size()];
					mPendingResources.toArray(files);
					mPendingResources.clear();
				}
				IWorkbenchWindow window = null;
				try {
					if (displayProgress)
						window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
				}
				catch (Exception exception) {
					window = null;
				}
				if (window != null) {
					window.run(true, true, new IRunnableWithProgress() {
						public void run(IProgressMonitor monitor) throws InvocationTargetException,
								InterruptedException {
							scanPendingChangedFiles(files, monitor);
							if (monitor != null && monitor.isCanceled()) {
								synchronized (mPendingResources) {
									mPendingResources.addAll(Arrays.asList(files));
								}
							}
						}
					});
				}
				else
					scanPendingChangedFiles(files, null);
			}
			catch (Exception exception) {
				SchemeScriptPlugin.logException("Error while updating user dictionary", exception);
			}
		}
	}

	private void scanPendingChangedFiles(final IFile[] files, IProgressMonitor monitor) {
		if (monitor != null)
			monitor.beginTask("Updating dictionary:", files.length);
		for (int i = 0; i < files.length; i++) {
			IFile file = files[i];
			if (monitor != null && monitor.isCanceled())
				break;
			scanSchemeFile(file, monitor);
			if (monitor != null)
				monitor.worked(1);
		}
		if (monitor != null)
			monitor.done();
	}

	private void scanSchemeFile(IFile file, IProgressMonitor monitor) {
		if (monitor != null) {
			monitor.subTask(file.getProject().getName() + " - " + file.getName());
		}
		updateDictionaryForResource(file);
		// TODO: Put this is its own ResourceChangeListener
		SchemeScriptPlugin.getReferencesManager().scanResourceForSymbols(file);
		
	}

	private void updateDictionaryForResource(IFile file) {
		Object object = KawaProxy.get("update-dictionary-for-file");
		if (object != null && object instanceof Procedure)
		{
			Procedure updateForFile = (Procedure) object;
			try {
				updateForFile.apply1(file);
			}
			catch (Throwable exception) {
				SchemeScriptPlugin.logException("Error while calling 'update-dictionary-for-file' for: " + file.getName(), exception);
			}
		}
	}

	protected void findSymbols(String name, List entries) {
		processPendingResources(true);
		List existingEntries = (List) mEntries.get(name);
		if (existingEntries != null) {
			for (Iterator iterator = existingEntries.iterator(); iterator.hasNext();) {
				SymbolEntry entry = (SymbolEntry) iterator.next();
				entries.add(entry);
			}
		}
	}

	protected void completeSymbols(String prefix, List entries) {
		processPendingResources(true);

		Enumeration enumeration = mEntries.keys();
		while (enumeration.hasMoreElements()) {
			String symbolName = (String) enumeration.nextElement();
			if (symbolName.startsWith(prefix)) {
				List existingEntries = (List) mEntries.get(symbolName);
				for (Iterator iterator = existingEntries.iterator(); iterator.hasNext();) {
					SymbolEntry entry = (SymbolEntry) iterator.next();
					entries.add(entry);
				}
			}
		}
	}

	public void resourceChanged(IResourceChangeEvent event) {
		synchronized (mPendingResources) {
			processResourceDelta(event.getDelta());
		}
	}

	private void processChangedResource(IFile file) {
		try {
			if (isSchemeSourceFile(file)) {
				if (!mPendingResources.contains(file))
					mPendingResources.add(file);
			}
		}
		catch (Exception exception) {
			SchemeScriptPlugin.logException("Error while updating user dictionary", exception);
		}
	}

	private boolean isSchemeSourceFile(IFile file) {
		String extension = file.getFileExtension();
		return extension != null && mSchemeExtensions.contains(extension);
	}

	private void processResourceDelta(IResourceDelta delta) {
		IResource resource = delta.getResource();
		int kind = delta.getKind();
		if (resource instanceof IFile
				&& (kind == IResourceDelta.ADDED || (kind == IResourceDelta.CHANGED && ((delta.getFlags() & IResourceDelta.CONTENT) != 0)))) {
			processChangedResource((IFile) resource);
		}
		IResourceDelta[] children = delta.getAffectedChildren();
		for (int i = 0; i < children.length; i++) {
			processResourceDelta(children[i]);
		}
	}

}