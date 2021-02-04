+++
title = "PlantUML"
author = ["Christopher James Hayward"]
lastmod = 2021-02-04T14:39:22-05:00
draft = false
+++

As described by the webiste[^fn:1]:

> **PlantUML** is a component that allows you to quickly write UML and other data diagrams, using a simple and intuitive language.


## Display JSON data {#display-json-data}

You can use the JSON format with PlantUML to visualize your data, to activate the feature the diagram must use the `@startjson` and `@endjson` tags, respectively[^fn:2].

```plantuml
@startjson
"Hello world!"
@endjson
```

{{< figure src="/ox-hugo/plantuml-display-json-data.png" >}}


### Simple example {#simple-example}

Here's an example of a `user` object, with an accompanying `address` and list of `phone` numbers:

```plantuml
@startjson
{
  "name": "Bob",
  "email": "bob@bob.com",
  "phone": [
    { "type": "work", "number": "555-1234" },
    { "type": "mobile", "number": "555-4321" }
  ],
  "address": {

  }
}
@endjson
```

{{< figure src="/ox-hugo/plantuml-display-json-data-simple-example.png" >}}


### Complex example {#complex-example}

Here is the example of a complex data structure[^fn:2], which comes from the Wikipedia page for JSON[^fn:3]:

```plantuml
@startjson
{
  "firstName": "John",
  "lastName": "Smith",
  "isAlive": true,
  "age": 27,
  "address": {
    "streetAddress": "21 2nd Street",
    "city": "New York",
    "state": "NY",
    "postalCode": "10021-3100"
  },
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    }
  ],
  "children": [],
  "spouse": null
}
@endjson
```

{{< figure src="/ox-hugo/plantuml-display-json-data-complex-example.png" >}}


## Display YAML data {#display-yaml-data}

Much like JSON, PlantUML can visualize YAML data using the `@startyaml`, and `@endyaml` keywords[^fn:4]:

```plantuml
@startyaml
fruit: Apple
size: Large
@endyaml
```

{{< figure src="/ox-hugo/plantuml-display-yaml-data.png" >}}


### Docker example {#docker-example}

Here's an example docker compose file running with a simple application structure[^fn:5]:

```plantuml
@startyaml
version: "3.9"

services:
  db:
    image: postgres
    environment:
      - POSTGRES_DB=postgres
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
  web:
    build: .
    command: python manage.py runserver 0.0.0.0:8000
    volumes:
      - .:/code
    ports:
      - "8000:8000"
    depends_on:
      - db
@endyaml
```

{{< figure src="/ox-hugo/plantuml-display-yaml-data-docker-example.png" >}}


## Sequence Diagram {#sequence-diagram}

Here's the complete example[^fn:6] showing many participants.

```plantuml
@startuml
/'
 ' Define the participant(s).
 '/
participant participant as 1
actor actor as 2
boundary boundary as 3
control control as 4
entity entity as 5
database database as 6
collections collections as 7
queue queue as 8
/'
 ' Draw a line to each participant(s).
 '/
1 -> 2 : To actor
1 -> 3 : To boundary
1 -> 4 : To control
1 -> 5 : To entity
1 -> 6 : To database
1 -> 7 : To collection
1 -> 8 : To queue
@enduml
```

{{< figure src="/ox-hugo/plantuml-sequence-diagram.png" >}}

Here's a list of all the available keywords:

-   actor
-   boundary
-   control
-   entity
-   database
-   collections
-   queue


## Resources {#resources}

[^fn:1]: PlantUML Website <https://plantuml.com>
[^fn:2]: PlantUML JSON Data <https://plantuml.com/json>
[^fn:3]: Wikipedia entry for JSON <https://en.wikipedia.org/wiki/JSON>
[^fn:4]: PlantUML YAML Data <https://plantuml.com/yaml>
[^fn:5]: Docker compose documentation <https://docs.docker.com/compose/django/>
[^fn:6]: PlantUML Sequence Diagrams <https://plantuml.com/sequence-diagram>
