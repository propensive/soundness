package soundness

export ambience.{Environment, EnvironmentError, EnvironmentVariable, Properties, PropertyAccess,
    SystemProperties, SystemProperty, SystemPropertyError, Xdg}

package systemProperties:
  export ambience.systemProperties.{empty, virtualMachine}

package environments:
  export ambience.environments.{empty, virtualMachine}

package workingDirectories:
  export rudiments.workingDirectories.virtualMachine

package homeDirectories:
  export rudiments.homeDirectories.virtualMachine
