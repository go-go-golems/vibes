package api

import (
	"context"
	"fmt"

	"github.com/github-api-project/internal/models"
	"github.com/shurcooL/graphql"
	"golang.org/x/oauth2"
)

// Client handles interactions with the GitHub GraphQL API
type Client struct {
	graphqlClient *graphql.Client
	organization  string
}

// NewClient creates a new API client with the given token and organization
func NewClient(token, organization string) *Client {
	src := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: token},
	)
	httpClient := oauth2.NewClient(context.Background(), src)
	
	return &Client{
		graphqlClient: graphql.NewClient("https://api.github.com/graphql", httpClient),
		organization:  organization,
	}
}

// GetOrganizationID retrieves the node ID of the organization
func (c *Client) GetOrganizationID() (string, error) {
	var query struct {
		Organization struct {
			ID graphql.String
		} `graphql:"organization(login: $login)"`
	}
	
	variables := map[string]interface{}{
		"login": graphql.String(c.organization),
	}
	
	err := c.graphqlClient.Query(context.Background(), &query, variables)
	if err != nil {
		return "", fmt.Errorf("failed to get organization ID: %w", err)
	}
	
	return string(query.Organization.ID), nil
}

// GetUserID retrieves the node ID of a user
func (c *Client) GetUserID(username string) (string, error) {
	var query struct {
		User struct {
			ID graphql.String
		} `graphql:"user(login: $login)"`
	}
	
	variables := map[string]interface{}{
		"login": graphql.String(username),
	}
	
	err := c.graphqlClient.Query(context.Background(), &query, variables)
	if err != nil {
		return "", fmt.Errorf("failed to get user ID: %w", err)
	}
	
	return string(query.User.ID), nil
}

// CreateProject creates a new project and returns its ID
func (c *Client) CreateProject(project models.Project, ownerID string) (string, error) {
	var mutation struct {
		CreateProjectV2 struct {
			ProjectV2 struct {
				ID graphql.String
			}
		} `graphql:"createProjectV2(input: $input)"`
	}
	
	input := map[string]interface{}{
		"ownerId":     graphql.ID(ownerID),
		"title":       graphql.String(project.Name),
		"description": graphql.String(project.Description),
		"public":      graphql.Boolean(project.Public),
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return "", fmt.Errorf("failed to create project: %w", err)
	}
	
	return string(mutation.CreateProjectV2.ProjectV2.ID), nil
}

// GetProject retrieves a project by its number
func (c *Client) GetProject(projectNumber int) (*models.Project, error) {
	var query struct {
		Organization struct {
			ProjectV2 struct {
				ID     graphql.String
				Title  graphql.String
				Public graphql.Boolean
				Fields struct {
					Nodes []struct {
						TypeName graphql.String `graphql:"__typename"`
						ID       graphql.String
						Name     graphql.String
						Options  []struct {
							ID   graphql.String
							Name graphql.String
						} `graphql:"... on ProjectV2SingleSelectField"`
					}
				} `graphql:"fields(first: 20)"`
			} `graphql:"projectV2(number: $number)"`
		} `graphql:"organization(login: $login)"`
	}
	
	variables := map[string]interface{}{
		"login":  graphql.String(c.organization),
		"number": graphql.Int(projectNumber),
	}
	
	err := c.graphqlClient.Query(context.Background(), &query, variables)
	if err != nil {
		return nil, fmt.Errorf("failed to get project: %w", err)
	}
	
	project := &models.Project{
		ID:     string(query.Organization.ProjectV2.ID),
		Name:   string(query.Organization.ProjectV2.Title),
		Public: bool(query.Organization.ProjectV2.Public),
	}
	
	// Extract columns (status field)
	for _, field := range query.Organization.ProjectV2.Fields.Nodes {
		if string(field.TypeName) == "ProjectV2SingleSelectField" && string(field.Name) == "Status" {
			for _, option := range field.Options {
				project.Columns = append(project.Columns, models.Column{
					Name: string(option.Name),
					ID:   string(option.ID),
				})
			}
			break
		}
	}
	
	return project, nil
}

// CreateIssue creates a new issue and returns its ID
func (c *Client) CreateIssue(issue models.Issue, repositoryID string) (string, error) {
	var mutation struct {
		CreateIssue struct {
			Issue struct {
				ID graphql.String
			}
		} `graphql:"createIssue(input: $input)"`
	}
	
	input := map[string]interface{}{
		"repositoryId": graphql.ID(repositoryID),
		"title":        graphql.String(issue.Title),
		"body":         graphql.String(issue.Body),
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return "", fmt.Errorf("failed to create issue: %w", err)
	}
	
	return string(mutation.CreateIssue.Issue.ID), nil
}

// CreateDraftIssue creates a new draft issue in a project and returns its ID
func (c *Client) CreateDraftIssue(issue models.Issue, projectID string) (string, error) {
	var mutation struct {
		AddProjectV2DraftIssue struct {
			ProjectItem struct {
				ID graphql.String
			}
		} `graphql:"addProjectV2DraftIssue(input: $input)"`
	}
	
	input := map[string]interface{}{
		"projectId": graphql.ID(projectID),
		"title":     graphql.String(issue.Title),
		"body":      graphql.String(issue.Body),
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return "", fmt.Errorf("failed to create draft issue: %w", err)
	}
	
	return string(mutation.AddProjectV2DraftIssue.ProjectItem.ID), nil
}

// AddIssueToProject adds an existing issue to a project and returns the project item ID
func (c *Client) AddIssueToProject(issueID, projectID string) (string, error) {
	var mutation struct {
		AddProjectV2ItemById struct {
			Item struct {
				ID graphql.String
			}
		} `graphql:"addProjectV2ItemById(input: $input)"`
	}
	
	input := map[string]interface{}{
		"projectId": graphql.ID(projectID),
		"contentId": graphql.ID(issueID),
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return "", fmt.Errorf("failed to add issue to project: %w", err)
	}
	
	return string(mutation.AddProjectV2ItemById.Item.ID), nil
}

// UpdateIssueStatus updates the status of an issue in a project
func (c *Client) UpdateIssueStatus(projectID, itemID, fieldID, optionID string) error {
	var mutation struct {
		UpdateProjectV2ItemFieldValue struct {
			ProjectV2Item struct {
				ID graphql.String
			}
		} `graphql:"updateProjectV2ItemFieldValue(input: $input)"`
	}
	
	input := map[string]interface{}{
		"projectId": graphql.ID(projectID),
		"itemId":    graphql.ID(itemID),
		"fieldId":   graphql.ID(fieldID),
		"value": map[string]interface{}{
			"singleSelectOptionId": graphql.String(optionID),
		},
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return fmt.Errorf("failed to update issue status: %w", err)
	}
	
	return nil
}

// AddSubIssue adds an existing issue as a sub-issue to a parent issue
func (c *Client) AddSubIssue(parentIssueID, subIssueID string) error {
	var mutation struct {
		LinkProjectV2ItemToSubItem struct {
			ParentItem struct {
				ID graphql.String
			}
			SubItem struct {
				ID graphql.String
			}
		} `graphql:"linkProjectV2ItemToSubItem(input: $input)"`
	}
	
	input := map[string]interface{}{
		"parentItemId": graphql.ID(parentIssueID),
		"subItemId":    graphql.ID(subIssueID),
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return fmt.Errorf("failed to add sub-issue: %w", err)
	}
	
	return nil
}

// AddComment adds a comment to an issue
func (c *Client) AddComment(issueID, body string) error {
	var mutation struct {
		AddComment struct {
			CommentEdge struct {
				Node struct {
					ID graphql.String
				}
			}
		} `graphql:"addComment(input: $input)"`
	}
	
	input := map[string]interface{}{
		"subjectId": graphql.ID(issueID),
		"body":      graphql.String(body),
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return fmt.Errorf("failed to add comment: %w", err)
	}
	
	return nil
}

// AddAssignees adds assignees to an issue
func (c *Client) AddAssignees(issueID string, assigneeIDs []string) error {
	var mutation struct {
		AddAssigneesToAssignable struct {
			Assignable struct {
				ID graphql.String
			}
		} `graphql:"addAssigneesToAssignable(input: $input)"`
	}
	
	// Convert string IDs to graphql.ID
	assigneeIDsGraphQL := make([]graphql.ID, len(assigneeIDs))
	for i, id := range assigneeIDs {
		assigneeIDsGraphQL[i] = graphql.ID(id)
	}
	
	input := map[string]interface{}{
		"assignableId": graphql.ID(issueID),
		"assigneeIds":  assigneeIDsGraphQL,
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return fmt.Errorf("failed to add assignees: %w", err)
	}
	
	return nil
}

// AddLabels adds labels to an issue
func (c *Client) AddLabels(issueID string, labelIDs []string) error {
	var mutation struct {
		AddLabelsToLabelable struct {
			Labelable struct {
				ID graphql.String
			}
		} `graphql:"addLabelsToLabelable(input: $input)"`
	}
	
	// Convert string IDs to graphql.ID
	labelIDsGraphQL := make([]graphql.ID, len(labelIDs))
	for i, id := range labelIDs {
		labelIDsGraphQL[i] = graphql.ID(id)
	}
	
	input := map[string]interface{}{
		"labelableId": graphql.ID(issueID),
		"labelIds":    labelIDsGraphQL,
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return fmt.Errorf("failed to add labels: %w", err)
	}
	
	return nil
}

// GetStatusField retrieves the status field ID for a project
func (c *Client) GetStatusField(projectID string) (string, map[string]string, error) {
	var query struct {
		Node struct {
			Project struct {
				Fields struct {
					Nodes []struct {
						TypeName graphql.String `graphql:"__typename"`
						ID       graphql.String
						Name     graphql.String
						Options  []struct {
							ID   graphql.String
							Name graphql.String
						} `graphql:"... on ProjectV2SingleSelectField"`
					}
				} `graphql:"fields(first: 20)"`
			} `graphql:"... on ProjectV2"`
		} `graphql:"node(id: $id)"`
	}
	
	variables := map[string]interface{}{
		"id": graphql.ID(projectID),
	}
	
	err := c.graphqlClient.Query(context.Background(), &query, variables)
	if err != nil {
		return "", nil, fmt.Errorf("failed to get status field: %w", err)
	}
	
	// Find the Status field
	for _, field := range query.Node.Project.Fields.Nodes {
		if string(field.TypeName) == "ProjectV2SingleSelectField" && string(field.Name) == "Status" {
			// Create a map of status names to option IDs
			statusMap := make(map[string]string)
			for _, option := range field.Options {
				statusMap[string(option.Name)] = string(option.ID)
			}
			return string(field.ID), statusMap, nil
		}
	}
	
	return "", nil, fmt.Errorf("status field not found")
}

// UpdateProject updates an existing project
func (c *Client) UpdateProject(project models.Project) error {
	var mutation struct {
		UpdateProjectV2 struct {
			ProjectV2 struct {
				ID graphql.String
			}
		} `graphql:"updateProjectV2(input: $input)"`
	}
	
	input := map[string]interface{}{
		"projectId":   graphql.ID(project.ID),
		"title":       graphql.String(project.Name),
		"description": graphql.String(project.Description),
		"public":      graphql.Boolean(project.Public),
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return fmt.Errorf("failed to update project: %w", err)
	}
	
	return nil
}

// CreateLabel creates a new label in a repository and returns its ID
func (c *Client) CreateLabel(repositoryID, name, color string) (string, error) {
	var mutation struct {
		CreateLabel struct {
			Label struct {
				ID graphql.String
			}
		} `graphql:"createLabel(input: $input)"`
	}
	
	input := map[string]interface{}{
		"repositoryId": graphql.ID(repositoryID),
		"name":         graphql.String(name),
		"color":        graphql.String(color),
	}
	
	variables := map[string]interface{}{
		"input": input,
	}
	
	err := c.graphqlClient.Mutate(context.Background(), &mutation, variables)
	if err != nil {
		return "", fmt.Errorf("failed to create label: %w", err)
	}
	
	return string(mutation.CreateLabel.Label.ID), nil
}
