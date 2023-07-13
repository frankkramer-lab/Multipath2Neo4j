# Multipath2Neo4j
Multipath2Neo4j serves as a general efficient framework, free of domain specific contexts, for collecting, modeling, visualizing and storing biological knowledge related to pathways.

# Description
Multipath2Neo4j is an R package developed under the latest version of R 4.1.1. It mainly uses four R packages: neo4r, rBiopaxParser, mully and Multipath.
The package first uses Multipath to download the list of all pathways, then parse them using rBiopaxParser. The obtained dataframe is then converted to a mully object, so mully project is involved in the whole process obviously. 
Finally, the constructed mully graph is sent to the Neo4j in order to be stored, which is done within the neo4r package.

![image](https://github.com/frankkramer-lab/Multipath2Neo4j/assets/45902776/e5088e16-c626-46ea-bf67-3fa49e4ac9a7)

# Package Installation
The package Multipath2Neo4j is available on GitHub at the following link: https://github.com/frankkramer-lab/Multipath2Neo4j
It can be installed directly from GitHub using the R package devtools. To do so, the following script should be called:

require(devtools)
install_github("frankkramer-lab/Multipath2Neo4j")
library(Multipath2Neo4j)

# Features

In this package, all the features of the mully project were reimplemented to adapt to the Neo4j graph database:

![image](https://github.com/frankkramer-lab/Multipath2Neo4j/assets/45902776/405d985c-dab8-4c0c-ae55-74ae57f02c26)

Below are the categories for all the functions created with their corresponding descriptions:

# Connection

• connectToNeo4j(): to establish a connection between R and Neo4j. The function takes the Neo4j server URL, username and database name as arguments and returns a Boolean value.
To check if the connection succeeded, connection$ping is used.

Data Preparation
• prepareNodesDataframeToCreate(): the function gets the list of all nodes of the mully object from the mully package and adjust the dataframe returned to be ready for creation process.

• splitDBIDs(): a private function to add, for each node, the list of its identifiers in all databases. This function is used by prepareNodesDataframeToCreate() function.

• prepareRelationshipsDataframeToCreate(): similar to prepareNodesDataframeToCreate(), the function gets the list of relationships of mully object from the mully package and adapts it for boosting the creation process.

# Creation

• createNode(): a function to create a node in a Neo4j database, with or without properties. It returns the response of the server.

• createRelationship(): likewise, createRelationship() creates a relationship, with or without attributes, in a Neo4j database by specifying the start and end nodes, and then returns the response of the server.

# Removal

• removeAllNodesOfLabel(): this function aims at removing a complete layer with its nodes from a Neo4j database. This function could be called in a loop to delete all layers.

• removeNode(): for removing a node from the database identified by a property in a key-value pair.

• removeRelationship(): to remove a relationship among two given nodes. If a property is given as argument, relations matching this filter will be removed, otherwise all edges between the two nodes will be deleted.

# Getters

• getLabelsCount(): for counting the number of labels present in the database.

• isLabel(): to check if the given label exists in the database. Note that the label name is case insensitive.

• getNodeAttributes(): it has double-usage. The first one is the extract the dataframe of a specific node identified with a property, with all its attributes, and the second one is to get the dataframe of all nodes, as well as their characteristics.

• getLabel(): to get all nodes of a given layer with their properties.

• getRelationshipAttributes(): similar to getNodeAttributes(), the method returns all the relationships with their attributes and nodes connected within if no property is given. Otherwise, only relations matching the given property key and value will be returned.

# Storage

• storeMully(): the function gets the biopax and mully objects and store them in the database, by benefiting from most of the aforementioned methods. The nodes are added, and the labels representing the layers are created automatically.
