FROM ruby:3.0-alpine

# Set working directory
WORKDIR /app

# Install build dependencies
RUN apk add --no-cache build-base sqlite-dev

# Copy Gemfile
COPY ruby-service/Gemfile Gemfile

# Bundle install
RUN bundle install

# Copy application code
COPY ruby-service/ ./

# Copy IDL files
COPY idl/ ./idl/

# Create db directory
RUN mkdir -p db

# Run the application
CMD ["ruby", "app.rb"]