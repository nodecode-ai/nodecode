Feature: Pinned cell formatting is reproducible
  Scenario: A clean cell materialization includes the committed formatting fix
    Given pristine cell sources at the pinned revision
    When cells-pull materializes the sources
    Then the committed dependency patch is applied
  Scenario: Repeating cells-pull preserves already patched sources
    Given already patched cell sources and matching bundle markers
    When cells-pull runs again
    Then every source byte remains unchanged
  Scenario: A changed upstream pin cannot silently reuse an obsolete patch
    Given a patch scoped to a different revision
    When cells-pull runs
    Then it fails with a patch revision diagnostic
  Scenario: A conflicting local change is never silently overwritten
    Given a matching bundle marker and conflicting source edits
    When cells-pull runs
    Then patch application fails and preserves those edits
  Scenario: Refreshing only one of two patched cells succeeds
    Given two patched cells at the same pin and one missing ASDF file
    When cells-pull refreshes only the incomplete cell
    Then both cells are patched and a repeat pull is byte-identical
  Scenario: Every old and new target is confined before writing
    Given traversal, absolute, undeclared or symlink patch targets
    When check or apply runs
    Then it fails without changing any source or outside file
  Scenario: A later conflict cannot partially apply an earlier file
    Given a pristine file and a second locally edited file
    When the formatting bridge preflights the patch
    Then it fails and preserves both files
