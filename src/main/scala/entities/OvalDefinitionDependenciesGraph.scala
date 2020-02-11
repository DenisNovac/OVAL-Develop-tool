package entities

case class OvalDefinitionDependenciesGraph(rootDefinition: OvalDefinition,
                                           externalDefinitions: Array[OvalDefinition],
                                           tests: Array[OvalTest],
                                           objects: Array[OvalObject],
                                           states: Array[OvalState],
                                           variables: Array[OvalVariable]
                                          )
