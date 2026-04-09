# Zeta Release Process

## Overview

This document outlines the process for creating new Zeta releases. The process is automated via GitHub Actions when version tags are pushed.

## Release Types

### Major Releases (v1.0.0, v2.0.0)
- Significant new features
- Breaking changes
- Major architectural improvements

### Minor Releases (v0.5.0, v0.6.0)
- New features (non-breaking)
- Performance improvements
- Significant bug fixes

### Patch Releases (v0.5.1, v0.5.2)
- Bug fixes only
- Security patches
- Documentation updates

## Release Checklist

### Pre-Release
- [ ] All tests pass
- [ ] Documentation updated
- [ ] Release notes prepared
- [ ] Version number updated in relevant files
- [ ] Code reviewed and approved

### Release Creation
- [ ] Create and push version tag: `git tag v0.5.0`
- [ ] Push tag to GitHub: `git push origin v0.5.0`
- [ ] GitHub Actions automatically creates release
- [ ] Verify release artifacts are correct
- [ ] Mark as "Latest" release if appropriate

### Post-Release
- [ ] Announce release on appropriate channels
- [ ] Update documentation websites
- [ ] Monitor for issues reported by users
- [ ] Begin planning next release

## Automated Release Workflow

The `.github/workflows/release.yml` workflow automatically:

1. **Triggers** when a version tag is pushed (e.g., `v0.5.0`)
2. **Creates release notes** from git history
3. **Generates GitHub release** with proper tagging
4. **Uploads source artifacts** as release assets
5. **Notifies** relevant parties (if configured)

## Manual Release Steps

If automated release fails, follow these manual steps:

1. **Create tag locally:**
   ```bash
   git tag v0.5.0
   git push origin v0.5.0
   ```

2. **Create GitHub release:**
   - Go to GitHub repository → Releases → Draft new release
   - Select the tag you just pushed
   - Add release notes (copy from `release_notes.md` if generated)
   - Upload any build artifacts
   - Publish release

3. **Verify release:**
   - Download release artifacts
   - Test installation and basic functionality
   - Update documentation links if needed

## Version Numbering

Follow [Semantic Versioning](https://semver.org/):
- **MAJOR** version for incompatible API changes
- **MINOR** version for added functionality (backwards compatible)
- **PATCH** version for backwards compatible bug fixes

## Release Notes Format

Release notes should include:

1. **Version number** and release date
2. **Highlights** - Major features and improvements
3. **Changes** - Detailed list of changes
4. **Breaking changes** (if any) with migration instructions
5. **Known issues** and workarounds
6. **Contributors** (optional)

## Quality Assurance

Before creating a release:

1. **Test on multiple platforms** (Linux, macOS, Windows if supported)
2. **Verify backward compatibility** with previous versions
3. **Check documentation** for accuracy
4. **Run full test suite** and ensure all tests pass
5. **Perform smoke tests** with example programs

## Emergency Releases

For critical security fixes:

1. **Create patch branch** from latest release
2. **Apply fix** and test thoroughly
3. **Create patch release** (e.g., v0.5.1)
4. **Notify users** of security implications
5. **Merge fix** back to main development branch