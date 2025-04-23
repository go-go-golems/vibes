# GitHub API Reference for Project Management

This document provides a reference for the GitHub API endpoints used in the GitHub Project Management tool.

## GraphQL API

The GitHub Project Management tool uses GitHub's GraphQL API for most operations. The GraphQL API provides a more flexible and efficient way to interact with GitHub compared to the REST API.

### Authentication

All GraphQL API requests require authentication using a Personal Access Token (PAT) with the appropriate scopes:
- `project` (for full access to projects)
- `repo` (for access to repositories and issues)

The token is passed in the Authorization header:

```
Authorization: bearer TOKEN
```

### Endpoint

All GraphQL API requests are sent to:

```
https://api.github.com/graphql
```

## Key Operations

### Getting Organization ID

```graphql
query {
  organization(login: "org-name") {
    id
  }
}
```

### Creating a Project

```graphql
mutation {
  createProjectV2(input: {
    ownerId: "OWNER_ID",
    title: "Project Title",
    description: "Project Description",
    public: true
  }) {
    projectV2 {
      id
    }
  }
}
```

### Getting Project Details

```graphql
query {
  organization(login: "org-name") {
    projectV2(number: 1) {
      id
      title
      public
      fields(first: 20) {
        nodes {
          ... on ProjectV2SingleSelectField {
            id
            name
            options {
              id
              name
            }
          }
        }
      }
    }
  }
}
```

### Creating a Draft Issue in a Project

```graphql
mutation {
  addProjectV2DraftIssue(input: {
    projectId: "PROJECT_ID",
    title: "Issue Title",
    body: "Issue Body"
  }) {
    projectItem {
      id
    }
  }
}
```

### Adding an Existing Issue to a Project

```graphql
mutation {
  addProjectV2ItemById(input: {
    projectId: "PROJECT_ID",
    contentId: "ISSUE_ID"
  }) {
    item {
      id
    }
  }
}
```

### Updating Issue Status

```graphql
mutation {
  updateProjectV2ItemFieldValue(input: {
    projectId: "PROJECT_ID",
    itemId: "ITEM_ID",
    fieldId: "FIELD_ID",
    value: {
      singleSelectOptionId: "OPTION_ID"
    }
  }) {
    projectV2Item {
      id
    }
  }
}
```

### Adding a Sub-Issue

```graphql
mutation {
  linkProjectV2ItemToSubItem(input: {
    parentItemId: "PARENT_ITEM_ID",
    subItemId: "SUB_ITEM_ID"
  }) {
    parentItem {
      id
    }
    subItem {
      id
    }
  }
}
```

### Adding a Comment to an Issue

```graphql
mutation {
  addComment(input: {
    subjectId: "ISSUE_ID",
    body: "Comment Body"
  }) {
    commentEdge {
      node {
        id
      }
    }
  }
}
```

### Adding Assignees to an Issue

```graphql
mutation {
  addAssigneesToAssignable(input: {
    assignableId: "ISSUE_ID",
    assigneeIds: ["USER_ID_1", "USER_ID_2"]
  }) {
    assignable {
      id
    }
  }
}
```

### Adding Labels to an Issue

```graphql
mutation {
  addLabelsToLabelable(input: {
    labelableId: "ISSUE_ID",
    labelIds: ["LABEL_ID_1", "LABEL_ID_2"]
  }) {
    labelable {
      id
    }
  }
}
```

## REST API

While the tool primarily uses the GraphQL API, some operations may be more convenient with the REST API.

### Getting User Information

```
GET /users/{username}
```

### Creating a Label

```
POST /repos/{owner}/{repo}/labels
```

Payload:
```json
{
  "name": "Label Name",
  "color": "color-hex-code",
  "description": "Label Description"
}
```

## Rate Limiting

GitHub API has rate limits that should be considered when making multiple requests:
- GraphQL API: 5,000 points per hour
- REST API: 5,000 requests per hour for authenticated requests

## Error Handling

The GitHub API returns detailed error messages that can help diagnose issues. Common errors include:
- Authentication errors (401)
- Permission errors (403)
- Not found errors (404)
- Validation errors (422)

## Resources

- [GitHub GraphQL API Documentation](https://docs.github.com/en/graphql)
- [GitHub REST API Documentation](https://docs.github.com/en/rest)
- [GitHub API Rate Limiting](https://docs.github.com/en/rest/overview/resources-in-the-rest-api#rate-limiting)
